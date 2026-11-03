"""Persistent Whisper backend with immediate, independent microphone capture."""
import asyncio
import importlib.util
from pathlib import Path
import socket
import subprocess
import threading
import time
import uuid

from .common import child_setup, pulse_client, stop_process

_spec = importlib.util.spec_from_file_location(
    'chatty_whisper_realtime', Path(__file__).resolve().parent.parent/'whisper-realtime.py')
realtime = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(realtime)


class Whisper:
    def __init__(self, config, notify, log):
        self.config, self.notify, self.log = config, notify, log
        self.server = None
        self.backend_task = None
        self.capture = None
        self.capture_id = None
        self.decode_task = None
        self.decode_tasks = set()
        self.readers = []
        self.inference_lock = threading.Lock()
        self.loop = asyncio.get_running_loop()
        self.closed = False
        self.stop_requested = set()
        self.replays = set()

    def start(self):
        self.backend_task = asyncio.create_task(self._backend())
        # Capture may start before model loading finishes.

    async def _backend(self):
        with socket.socket() as sock:
            sock.bind(('127.0.0.1', 0))
            self.port = sock.getsockname()[1]
        self.prefix = '/chatty-'+uuid.uuid4().hex
        config = self.config
        self.server = await asyncio.create_subprocess_exec(
            config['server'], '--model', config['model'], '--language', 'es',
            '--threads', str(config.get('threads', 4)), '--host', '127.0.0.1',
            '--port', str(self.port), '--request-path', self.prefix,
            '--vad', '--vad-model', config['vad_model'], '--beam-size', '5',
            '--best-of', '5', '--suppress-nst', stdin=asyncio.subprocess.DEVNULL,
            stdout=self.log, stderr=self.log, preexec_fn=child_setup)
        deadline = time.monotonic()+40
        while time.monotonic() < deadline:
            if self.server.returncode is not None:
                raise RuntimeError('Whisper server exited while loading; inspect the Chatty log')
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', self.port)
                writer.write(f'GET {self.prefix}/health HTTP/1.0\r\nHost: localhost\r\n\r\n'.encode())
                await writer.drain()
                status = await asyncio.wait_for(reader.readline(), 1)
                writer.close()
                await writer.wait_closed()
                if b' 200 ' in status:
                    self.notify('whisper-ready', backend_pid=self.server.pid)
                    return
            except (OSError, asyncio.TimeoutError):
                pass
            await asyncio.sleep(.05)
        raise TimeoutError('Whisper did not become ready within 40 seconds')

    async def record(self, capture_id, audio_file=None):
        self.cancel_capture()
        config = self.config
        pacat = pulse_client()
        if audio_file:
            inputs = ['-re', '-i', audio_file]
        elif config.get('input_format', 'pulse') == 'pulse':
            inputs = ['-f', 'pulse', '-sample_rate', '16000', '-channels', '1',
                      '-fragment_size', '3200', '-probesize', '32', '-analyzeduration', '1',
                      '-i', config.get('input_device', 'default')]
        else:
            inputs = ['-f', config['input_format'], '-i', config.get('input_device', 'default')]
        command = ['ffmpeg', '-nostdin', '-hide_banner', '-loglevel', 'error', *inputs,
                   '-ac', '1', '-ar', '16000', '-f', 's16le', '-flush_packets', '1', 'pipe:1']
        if not audio_file and config.get('input_format', 'pulse') == 'pulse' and pacat:
            command = [pacat, '--record', '--raw', '--format=s16le', '--rate=16000',
                       '--channels=1', '--latency-msec=40', '--client-name=Chatty Codex',
                       '--stream-name=Chatty microphone']
            device = config.get('input_device', 'default')
            if device and device != 'default': command.append('--device='+device)
        self.capture = subprocess.Popen(
            command,
            stdout=subprocess.PIPE, stdin=subprocess.DEVNULL, stderr=self.log,
            preexec_fn=child_setup)
        self.capture_id = capture_id
        if audio_file: self.replays.add(capture_id)
        process = self.capture
        audio = realtime.AudioBuffer()
        owner = self
        class CaptureStream:
            announced = False
            def read(self, size):
                data = process.stdout.read(size)
                if data and not self.announced:
                    self.announced = True
                    def ready():
                        if (capture_id == owner.capture_id and not owner.closed
                                and capture_id not in owner.stop_requested):
                            owner.notify('capture-ready', capture_id=capture_id, recording_pid=process.pid)
                    owner.loop.call_soon_threadsafe(ready)
                return data
        reader = threading.Thread(target=audio.read, args=(CaptureStream(),), daemon=True)
        reader.start()
        self.readers.append(reader)
        self.notify('recording', capture_id=capture_id, recording_pid=process.pid)
        self.decode_task = asyncio.create_task(self._decode(capture_id, process, audio))
        self.decode_tasks.add(self.decode_task)
        self.decode_task.add_done_callback(self.decode_tasks.discard)

    async def _decode(self, capture_id, process, audio):
        committed = []
        def event(kind, **fields):
            if kind == 'final':
                committed.append(fields['text'])
            text = ' '.join([*committed, fields['text'] if kind == 'partial' else '']).strip()
            def deliver():
                if not self.closed and capture_id == self.capture_id:
                    self.notify('transcript', capture_id=capture_id, text=text, partial=True)
            self.loop.call_soon_threadsafe(deliver)
        def inference(pcm):
            with self.inference_lock:
                return realtime.inference(self.port, self.prefix, 'es', pcm, False)
        try:
            await self.backend_task
            await asyncio.to_thread(realtime.transcribe_stream, audio, inference,
                                    self.config.get('step', 2), self.config.get('window', 30),
                                    self.config.get('overlap', 3), event)
            await asyncio.to_thread(process.wait)
            text = ' '.join(committed).strip()
            if capture_id == self.capture_id and not self.closed:
                self.capture = None
                if capture_id not in self.stop_requested and capture_id not in self.replays:
                    self.notify('transcription-error', capture_id=capture_id, transcript=text,
                                message='Microphone capture ended unexpectedly. Nothing was sent; start another recording.')
                elif not text:
                    self.notify('transcription-error', capture_id=capture_id,
                                message='No speech was recognized. Start another recording.')
                else:
                    self.notify('transcription-complete', capture_id=capture_id, text=text)
        except asyncio.CancelledError:
            raise
        except Exception as error:
            if capture_id == self.capture_id and not self.closed:
                self.notify('transcription-error', capture_id=capture_id, message=str(error))
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    await asyncio.wait_for(asyncio.to_thread(process.wait), 3)
                except asyncio.TimeoutError:
                    process.kill()
            process.stdout.close()

    def stop_recording(self):
        if self.capture_id: self.stop_requested.add(self.capture_id)
        if self.capture and self.capture.poll() is None:
            self.capture.terminate()
        self.notify('transcribing', capture_id=self.capture_id)

    def cancel_capture(self):
        self.capture_id = None
        if self.capture and self.capture.poll() is None:
            self.capture.terminate()
        self.capture = None

    async def close(self):
        self.closed = True
        self.cancel_capture()
        if self.backend_task:
            self.backend_task.cancel()
            await asyncio.gather(self.backend_task, return_exceptions=True)
        await stop_process(self.server)
        if self.decode_tasks:
            await asyncio.gather(*self.decode_tasks, return_exceptions=True)
