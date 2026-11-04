"""Small process and text helpers shared by the voice workers."""
import asyncio
import json
import os
from pathlib import Path
import re
import signal
import shutil

CACHE = Path.home() / '.cache/codex-chatty-tts'
SYSTEM_KOKORO = Path('/opt/kokoro.cpp')
ENTRY = Path(__file__).resolve().parent.parent / 'codex-chatty.py'
PARENT_PID = os.getpid()


def kokoro_paths(config):
    """Prefer the container installation; retain explicit and legacy paths."""
    installed = (SYSTEM_KOKORO/'build/libkokoro.so').is_file()
    source = SYSTEM_KOKORO if installed else CACHE/'kokoro.cpp'
    models = SYSTEM_KOKORO/'models' if installed else CACHE/'models/kokoro-cpp'
    return {key:str(config.get(key, default)) for key,default in {
        'library': source/'build/libkokoro.so',
        'model': models/'kokoro-82m-f16.gguf',
        'voice': models/'kokoro-voice-af_heart.gguf',
    }.items()}


def pulse_client():
    """Use the installed or research-cached native Pulse client, if available."""
    installed = shutil.which('pacat')
    cached = CACHE/'deps/root/usr/bin/pacat'
    return installed or (str(cached) if cached.is_file() else None)


def emit(event, **fields):
    print(json.dumps(dict(event=event, **fields), ensure_ascii=False), flush=True)


async def stop_process(process):
    if process is None or process.returncode is not None:
        return
    process.terminate()
    try:
        await asyncio.wait_for(process.wait(), 3)
    except asyncio.TimeoutError:
        process.kill()
        await process.wait()


def child_setup():
    if sys_platform_linux():
        import ctypes
        ctypes.CDLL(None).prctl(1, signal.SIGTERM)
        if os.getppid() != PARENT_PID:
            os.kill(os.getpid(), signal.SIGTERM)


def sys_platform_linux():
    import sys
    return sys.platform.startswith('linux')


class PhraseBuffer:
    """Release a short first sentence, then natural chunks of bounded length."""
    def __init__(self, maximum=240):
        self.maximum = maximum
        self.pending = ''
        self.first = True

    def feed(self, delta, final=False):
        self.pending += delta
        output = []
        while self.pending.strip():
            sentence_ends = [m.end() for m in re.finditer(r'[.!?](?:["\u201d\u2019])?(?=\s|$)', self.pending)]
            # A period at the end of a delta may still be a decimal/abbreviation.
            if not final and sentence_ends and sentence_ends[-1] == len(self.pending):
                sentence_ends.pop()
            cut = None
            if self.first and sentence_ends:
                cut = sentence_ends[0]
            elif len(self.pending) >= self.maximum:
                eligible = [end for end in sentence_ends if end <= self.maximum]
                cut = eligible[-1] if eligible else self.pending.rfind(' ', 0, self.maximum + 1)
                if cut <= 0:
                    cut = self.maximum
            elif final:
                cut = len(self.pending)
            if cut is None:
                break
            piece = self.pending[:cut].strip()
            self.pending = self.pending[cut:].lstrip()
            if piece:
                output.append(piece)
                self.first = False
        return output
