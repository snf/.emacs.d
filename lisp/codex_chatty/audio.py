"""Asynchronous synthesis and playback, with immediate turn invalidation."""
import asyncio
import base64
import json
import os
from pathlib import Path
import sys
import time
import uuid

from .common import CACHE, ENTRY, child_setup, kokoro_paths, pulse_client, stop_process


def playback_command(config):
    if config.get('player_command'):
        return config['player_command']
    pacat = pulse_client()
    if pacat:
        return [pacat, '--playback', '--raw', '--format=s16le', '--rate=24000',
                '--channels=1', '--latency-msec=40', '--client-name=Chatty Codex',
                '--stream-name=Chatty speech']
    if os.environ.get('PULSE_SERVER'):
        return ['ffmpeg', '-nostdin', '-hide_banner', '-loglevel', 'error',
                '-probesize', '32', '-analyzeduration', '1', '-f', 's16le',
                '-ar', '24000', '-ac', '1', '-i', 'pipe:0',
                '-f', 'pulse', '-buffer_duration', '40', 'Chatty']
    return ['pw-cat', '--playback', '--raw', '--rate', '24000',
            '--channels', '1', '--format', 's16', '--latency', '40ms', '-']


class Speech:
    def __init__(self, config, notify, log):
        self.config, self.notify, self.log = config, notify, log
        self.process = self.player = None
        self.queue = asyncio.Queue()
        self.pcm_queue = asyncio.Queue(maxsize=3)
        self.pending = {}
        self.ready = asyncio.get_running_loop().create_future()
        self.epoch = 0
        self.disabled = config.get('disabled', False)
        self.reader = self.consumer = self.writer = None
        self.started = 0
        self.first_audio = False
        self.capture = None
        self.closing = False

    async def start(self):
        if self.disabled:
            self.ready.set_result(None)
        else:
            env = os.environ.copy()
            deps = CACHE/'deps/root/usr/lib/x86_64-linux-gnu'
            if Path(kokoro_paths(self.config)['library']).is_relative_to(CACHE) and deps.is_dir():
                env['LD_LIBRARY_PATH'] = str(deps) + (':'+env['LD_LIBRARY_PATH'] if env.get('LD_LIBRARY_PATH') else '')
            env['GGML_VK_ALLOW_GRAPHICS_QUEUE'] = '1'
            self.process = await asyncio.create_subprocess_exec(
                sys.executable, str(ENTRY), '--tts-worker', stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE, stderr=self.log, env=env,
                limit=16*1024*1024, preexec_fn=child_setup)
            self.process.stdin.write((json.dumps(self.config)+'\n').encode())
            await self.process.stdin.drain()
            self.reader = asyncio.create_task(self._read())
        self.consumer = asyncio.create_task(self._consume())
        self.writer = asyncio.create_task(self._play())

    async def _read(self):
        error = None
        try:
            async for line in self.process.stdout:
                message = json.loads(line)
                if message['event'] == 'ready':
                    if not self.ready.done():
                        self.ready.set_result(message)
                    self.notify('tts-ready', **{k:v for k,v in message.items() if k != 'event'})
                elif message['event'] == 'fatal':
                    raise RuntimeError(message['message'])
                else:
                    future = self.pending.pop(message.get('job'), None)
                    if future and not future.done():
                        if message['event'] == 'error':
                            future.set_exception(RuntimeError(message['message']))
                        else:
                            future.set_result(message)
        except Exception as exception:
            error = exception
        finally:
            error = error or RuntimeError('Kokoro worker exited')
            if not self.ready.done():
                self.ready.set_exception(error)
                self.ready.exception()
            if not self.closing:
                self.notify('speech-error', epoch=self.epoch, message=str(error))
            for future in self.pending.values():
                if not future.done():
                    future.set_exception(error)
            self.pending.clear()

    def begin(self, epoch):
        self.epoch = epoch
        self.started = time.monotonic()
        self.first_audio = False

    def say(self, text, epoch):
        if text.strip():
            self.queue.put_nowait((epoch, text, None))

    async def finish(self, epoch):
        future = asyncio.get_running_loop().create_future()
        self.queue.put_nowait((epoch, None, future))
        return await future

    async def cancel(self, epoch):
        self.epoch = epoch
        await stop_process(self.player)
        self.player = None
        while not self.queue.empty():
            _, _, marker = self.queue.get_nowait()
            if marker and not marker.done():
                marker.set_result(False)
            self.queue.task_done()
        while not self.pcm_queue.empty():
            _, _, marker = self.pcm_queue.get_nowait()
            if marker and not marker.done(): marker.set_result(False)
            self.pcm_queue.task_done()

    async def _consume(self):
        while True:
            epoch, text, marker = await self.queue.get()
            try:
                if epoch != self.epoch:
                    if marker and not marker.done(): marker.set_result(False)
                    continue
                if marker:
                    await self.pcm_queue.put((epoch, None, marker))
                    continue
                if self.disabled:
                    continue
                await self.ready
                job = uuid.uuid4().hex
                future = asyncio.get_running_loop().create_future()
                self.pending[job] = future
                self.process.stdin.write((json.dumps(dict(job=job, text=text))+'\n').encode())
                await self.process.stdin.drain()
                message = await future
                if epoch != self.epoch:
                    continue
                pcm = base64.b64decode(message['pcm'], validate=True)
                if not self.first_audio:
                    self.first_audio = True
                    self.notify('first-audio', epoch=epoch, seconds=time.monotonic()-self.started)
                await self.pcm_queue.put((epoch, pcm, None))
                self.notify('audio-chunk', epoch=epoch, duration=message['duration'],
                            generation_seconds=message['generation_seconds'])
            except asyncio.CancelledError:
                raise
            except Exception as error:
                if marker and not marker.done(): marker.set_result(False)
                self.notify('speech-error', epoch=epoch, message=str(error))
                # Discussion and accepted handoff status survive an audio error.
            finally:
                self.queue.task_done()

    async def _play(self):
        while True:
            epoch, pcm, marker = await self.pcm_queue.get()
            try:
                if epoch != self.epoch:
                    if marker and not marker.done(): marker.set_result(False)
                    continue
                if marker:
                    if self.player:
                        self.player.stdin.close()
                        code = await self.player.wait()
                        self.player = None
                        if code: raise RuntimeError(f'Audio output exited with status {code}')
                    self.notify('playback-complete', epoch=epoch)
                    if not marker.done(): marker.set_result(True)
                    continue
                if not self.player:
                    command = playback_command(self.config)
                    self.player = await asyncio.create_subprocess_exec(
                        *command, stdin=asyncio.subprocess.PIPE,
                        stdout=asyncio.subprocess.DEVNULL, stderr=self.log, preexec_fn=child_setup)
                self.player.stdin.write(pcm)
                await self.player.stdin.drain()
            except asyncio.CancelledError:
                raise
            except Exception as error:
                if marker and not marker.done(): marker.set_result(False)
                if epoch == self.epoch:
                    self.notify('speech-error', epoch=epoch, message=str(error))
                await stop_process(self.player)
                self.player = None
            finally:
                self.pcm_queue.task_done()

    async def close(self):
        self.closing = True
        await self.cancel(self.epoch+1)
        if self.consumer:
            self.consumer.cancel()
            await asyncio.gather(self.consumer, return_exceptions=True)
        if self.writer:
            self.writer.cancel()
            await asyncio.gather(self.writer, return_exceptions=True)
        await stop_process(self.process)
        if self.reader:
            await asyncio.gather(self.reader, return_exceptions=True)
