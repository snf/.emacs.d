"""Persistent Kokoro C API worker. One JSON request produces one PCM chunk."""
import array
import base64
import ctypes as C
import json
import math
import os
from pathlib import Path
import sys
import time

from .common import CACHE, emit, kokoro_paths


class Params(C.Structure):
    _fields_ = [('abi_version', C.c_int), ('n_threads', C.c_int),
                ('verbosity', C.c_int), ('length_scale', C.c_float),
                ('espeak_lang', C.c_char * 32), ('backend', C.c_int), ('device', C.c_int)]


class Audio(C.Structure):
    _fields_ = [('samples', C.POINTER(C.c_float)), ('n_samples', C.c_int),
                ('sample_rate', C.c_int), ('channels', C.c_int)]


class Kokoro:
    def __init__(self, config):
        # Must precede even backend enumeration. This worker owns its GGML state.
        os.environ['GGML_VK_ALLOW_GRAPHICS_QUEUE'] = '1'
        os.environ['KOKORO_SEED'] = str(config.get('seed', 42))
        self.paths = kokoro_paths(config)
        library = Path(self.paths['library'])
        data_parents = [library.parent.parent]
        if library.is_relative_to(CACHE):
            data_parents.append(CACHE/'deps/root/usr/lib/x86_64-linux-gnu')
        for parent in data_parents:
            if (parent/'espeak-ng-data').is_dir():
                os.environ.setdefault('KOKORO_ESPEAK_DATA_PATH', str(parent))
                break
        self.lib = lib = C.CDLL(str(library))
        lib.kokoro_context_default_params.restype = Params
        lib.kokoro_init_from_file.argtypes = [C.c_char_p, Params]
        lib.kokoro_init_from_file.restype = C.c_void_p
        lib.kokoro_load_voice_pack.argtypes = [C.c_void_p, C.c_char_p]
        lib.kokoro_load_voice_pack.restype = C.c_int
        lib.kokoro_synthesize_audio.argtypes = [C.c_void_p, C.c_char_p, C.POINTER(Audio)]
        lib.kokoro_synthesize_audio.restype = C.c_int
        lib.kokoro_audio_free.argtypes = [C.POINTER(Audio)]
        lib.kokoro_free.argtypes = [C.c_void_p]
        lib.kokoro_last_error.restype = C.c_char_p
        lib.kokoro_backend_name.argtypes = [C.c_void_p]
        lib.kokoro_backend_name.restype = C.c_char_p
        params = lib.kokoro_context_default_params()
        params.n_threads = config.get('threads', 4)
        params.backend = 2  # Vulkan only; never AUTO/CPU.
        params.device = config.get('device', 0)
        params.length_scale = 1 / config.get('speed', 1.526)
        self.context = lib.kokoro_init_from_file(
            self.paths['model'].encode(), params)
        if not self.context:
            raise RuntimeError(lib.kokoro_last_error().decode())
        try:
            voice = self.paths['voice']
            if lib.kokoro_load_voice_pack(self.context, voice.encode()) != 0:
                raise RuntimeError(lib.kokoro_last_error().decode())
            if lib.kokoro_backend_name(self.context) != b'Vulkan':
                raise RuntimeError('Kokoro did not select Vulkan')
        except BaseException:
            self.close()
            raise

    def synthesize(self, text):
        audio = Audio()
        if self.lib.kokoro_synthesize_audio(self.context, text.encode(), C.byref(audio)) != 0:
            raise RuntimeError(self.lib.kokoro_last_error().decode())
        try:
            if audio.sample_rate != 24000 or audio.channels != 1 or audio.n_samples < 1:
                raise RuntimeError('Unexpected Kokoro audio format')
            samples = array.array('f', C.string_at(audio.samples, audio.n_samples * 4))
            if not all(math.isfinite(x) for x in samples):
                raise RuntimeError('Non-finite Kokoro audio')
            pcm = array.array('h', (max(-32767, min(32767, int(x * 32767))) for x in samples))
            if sys.byteorder != 'little':
                pcm.byteswap()
            return pcm.tobytes()
        finally:
            self.lib.kokoro_audio_free(C.byref(audio))

    def close(self):
        if self.context:
            self.lib.kokoro_free(self.context)
            self.context = None


def main():
    model = None
    try:
        first = sys.stdin.readline()
        if not first:
            return
        config = json.loads(first)
        started = time.monotonic()
        model = Kokoro(config)
        model.synthesize('The voice conversation is ready.')  # Silent warmup.
        emit('ready', backend='Vulkan', sample_rate=24000,
             warmup_seconds=time.monotonic() - started, **model.paths)
        for line in sys.stdin:
            request = json.loads(line)
            if request.get('command') == 'close':
                break
            job = request.get('job')
            started = time.monotonic()
            try:
                pcm = model.synthesize(request['text'])
                emit('audio', job=job, pcm=base64.b64encode(pcm).decode(),
                     sample_rate=24000, duration=len(pcm)/48000,
                     generation_seconds=time.monotonic()-started)
            except Exception as error:
                emit('error', job=job, message=str(error))
    except Exception as error:
        emit('fatal', message=str(error))
        raise SystemExit(1)
    finally:
        if model:
            model.close()
