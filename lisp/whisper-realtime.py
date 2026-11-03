"""Incremental local Whisper transcription; stdout contains JSON events only."""
import argparse
import ctypes
import http.client
import io
import json
import os
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time
import uuid
import wave

RATE = 16000
BYTES_PER_SECOND = RATE * 2
PARENT_PID = os.getpid()
OUTPUT_LOCK = threading.Lock()


def emit(event, **fields):
    with OUTPUT_LOCK:
        print(json.dumps(dict(event=event, **fields), ensure_ascii=False), flush=True)


def child_setup():
    # Cancelled/killed Emacs jobs must not leave a microphone capture behind.
    if sys.platform.startswith('linux'):
        ctypes.CDLL(None).prctl(1, signal.SIGTERM)
        if os.getppid() != PARENT_PID:
            os.kill(os.getpid(), signal.SIGTERM)


def stop_process(process):
    if process is not None and process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()


class AudioBuffer:
    def __init__(self):
        self.data = bytearray()
        self.done = False
        self.condition = threading.Condition()

    def read(self, stream, on_done=None):
        try:
            while chunk := stream.read(3200):
                with self.condition:
                    self.data.extend(chunk)
                    self.condition.notify_all()
        finally:
            with self.condition:
                self.done = True
                self.condition.notify_all()
            if on_done:
                on_done()


def transcribe_stream(audio, transcribe, step, window, overlap, output=emit):
    """Keep an unfinished tail; advance audio only at Whisper segment boundaries."""
    block_bytes = int(window * BYTES_PER_SECOND)
    overlap_bytes = int(overlap * BYTES_PER_SECOND)
    processed = 0
    next_update = time.monotonic() + step
    def text(segments):
        return ' '.join(' '.join(segment['text'] for segment in segments).split())
    while True:
        with audio.condition:
            available = len(audio.data)
            if audio.done and available == 0:
                return
            if not audio.done and available < block_bytes and (
                    available <= processed or time.monotonic() < next_update):
                audio.condition.wait(timeout=0.1)
                continue
            size = min(available, block_bytes)
            pcm = bytes(audio.data[:size])
        segments = transcribe(pcm)
        with audio.condition:
            # If capture ended during inference, reuse this result when it already
            # includes every sample instead of decoding the same audio again.
            last = audio.done and size == len(audio.data)
        if last:
            output('final', text=text(segments))
            return
        if size == block_bytes:
            safe_end = (size-overlap_bytes) / BYTES_PER_SECOND
            count = 0
            for segment in segments:
                if segment['end'] > safe_end:
                    break
                count += 1
            # A single long segment may fill the whole window.  It still needs
            # to advance so capture cannot build an unbounded decoding backlog.
            if segments and count == 0:
                count = 1
            end = segments[count-1]['end'] if count else safe_end
            consumed = min(size, max(2, int(end*RATE)*2))
            with audio.condition:
                del audio.data[:consumed]
            output('final', text=text(segments[:count]))
            output('partial', text=text(segments[count:]))
            processed = 0
        else:
            output('partial', text=text(segments))
            processed = size
        next_update = time.monotonic() + step


def inference(port, prefix, language, pcm, translate=False):
    wav = io.BytesIO()
    with wave.open(wav, 'wb') as stream:
        stream.setnchannels(1)
        stream.setsampwidth(2)
        stream.setframerate(RATE)
        stream.writeframes(pcm + b'\0' * max(0, BYTES_PER_SECOND-len(pcm)))
    boundary = 'whisper-' + uuid.uuid4().hex
    parts = []
    for name, value in {'language': language, 'translate': str(translate).lower(), 'response_format': 'verbose_json',
                        'no_context': 'true', 'no_timestamps': 'false', 'token_timestamps': 'false',
                        'no_language_probabilities': 'true', 'vad': 'true'}.items():
        parts.append(f'--{boundary}\r\nContent-Disposition: form-data; name="{name}"\r\n\r\n{value}\r\n'.encode())
    parts += [f'--{boundary}\r\nContent-Disposition: form-data; name="file"; filename="live.wav"\r\nContent-Type: audio/wav\r\n\r\n'.encode(),
              wav.getvalue(), f'\r\n--{boundary}--\r\n'.encode()]
    connection = http.client.HTTPConnection('127.0.0.1', port, timeout=30)
    try:
        connection.request('POST', prefix+'/inference', body=b''.join(parts),
                           headers={'Content-Type': 'multipart/form-data; boundary='+boundary})
        response = connection.getresponse()
        content = response.read()
        if response.status != 200:
            raise RuntimeError(f'Whisper inference failed (HTTP {response.status})')
        return json.loads(content)['segments']
    finally:
        connection.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--server', required=True)
    parser.add_argument('--model', required=True)
    parser.add_argument('--vad-model', required=True)
    parser.add_argument('--language', default='es')
    parser.add_argument('--translate', action='store_true')
    parser.add_argument('--threads', type=int, default=4)
    parser.add_argument('--input-format', default='pulse')
    parser.add_argument('--input-device', default='default')
    parser.add_argument('--step', type=float, default=10)
    parser.add_argument('--window', type=float, default=30)
    parser.add_argument('--overlap', type=float, default=3,
                        help='Seconds of unfinished tail to keep when committing full windows')
    parser.add_argument('--audio-file', help='Replay a file in real time for testing, without opening a microphone')
    parser.add_argument('--save-audio', help='Keep a recoverable WAV recording until the transcript is delivered')
    parser.add_argument('--duration', type=float, help='Optional recording time limit in seconds')
    args = parser.parse_args()
    if not 0 < args.step <= args.window or not 0 <= args.overlap < args.window:
        parser.error('Require 0 < step <= window and 0 <= overlap < window')
    stopping = threading.Event()
    capture = server = None

    def finish(_signal, _frame):
        stopping.set()
        if capture is not None and capture.poll() is None:
            capture.terminate()

    def cancel(signum, frame):
        finish(signum, frame)
        raise SystemExit(0)

    signal.signal(signal.SIGINT, finish)
    signal.signal(signal.SIGTERM, cancel)
    with tempfile.TemporaryFile() as server_log:
        try:
            with socket.socket() as reservation:
                reservation.bind(('127.0.0.1', 0))
                port = reservation.getsockname()[1]
            prefix = '/live-' + uuid.uuid4().hex
            emit('status', text='Loading Whisper '+args.language+'…')
            server = subprocess.Popen(
                [args.server, '--model', args.model, '--language', args.language,
                 '--threads', str(args.threads), '--host', '127.0.0.1', '--port', str(port),
                 '--request-path', prefix, '--vad', '--vad-model', args.vad_model,
                 '--beam-size', '5', '--best-of', '5', '--suppress-nst'],
                stdout=server_log, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL,
                preexec_fn=child_setup, start_new_session=True)
            deadline = time.monotonic() + 30
            while not stopping.is_set():
                if server.poll() is not None or time.monotonic() >= deadline:
                    raise RuntimeError('Could not start the local Whisper backend; see live transcription logs')
                connection = http.client.HTTPConnection('127.0.0.1', port, timeout=0.5)
                try:
                    connection.request('GET', prefix+'/health')
                    response = connection.getresponse()
                    response.read()
                    if response.status == 200:
                        break
                except OSError:
                    pass
                finally:
                    connection.close()
                stopping.wait(0.05)
            if stopping.is_set():
                emit('stopped')
                return
            log = os.pread(server_log.fileno(), 65536, 0).decode(errors='replace')
            backend = 'GPU' if 'whisper_backend_init_gpu: using ' in log else 'CPU'
            input_args = ['-re', '-i', args.audio_file] if args.audio_file else [
                '-f', args.input_format, '-i', args.input_device]
            duration = ['-t', str(args.duration)] if args.duration is not None else []
            save_audio = [*duration, '-ac', '1', '-ar', str(RATE), '-f', 'wav', args.save_audio] if args.save_audio else []
            capture = subprocess.Popen(
                ['ffmpeg', '-nostdin', '-hide_banner', '-loglevel', 'error', '-y', *input_args,
                 *duration, '-ac', '1', '-ar', str(RATE), '-f', 's16le', 'pipe:1', *save_audio],
                stdout=subprocess.PIPE, stderr=sys.stderr, stdin=subprocess.DEVNULL,
                preexec_fn=child_setup, start_new_session=True)
            audio = AudioBuffer()
            emit('ready', recording_pid=capture.pid, backend_pid=server.pid, backend=backend)
            reader = threading.Thread(target=audio.read,
                                      args=(capture.stdout, lambda: emit('capture-ended')), daemon=True)
            reader.start()
            transcribe_stream(audio, lambda pcm: inference(port, prefix, args.language, pcm, args.translate),
                              args.step, args.window, args.overlap)
            reader.join(timeout=2)
            if capture.wait(timeout=2) and not stopping.is_set():
                raise RuntimeError('Audio capture failed; check the selected microphone and live transcription logs')
            emit('stopped')
        except Exception as error:
            emit('error', message=str(error))
            raise SystemExit(1)
        finally:
            stop_process(capture)
            stop_process(server)
            server_log.seek(0)
            sys.stderr.write(server_log.read().decode(errors='replace'))


if __name__ == '__main__':
    main()
