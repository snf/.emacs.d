"""Streaming buffer tests without microphone access or model inference."""
import importlib.util
from pathlib import Path
import threading
import unittest

spec = importlib.util.spec_from_file_location(
    'whisper_realtime', Path(__file__).resolve().parents[1] / 'lisp/whisper-realtime.py')
live = importlib.util.module_from_spec(spec)
spec.loader.exec_module(live)


class RealtimeTests(unittest.TestCase):
    @staticmethod
    def segment(text, end):
        return dict(text=text, end=end)

    def test_audio_advances_at_completed_segments_and_keeps_the_unfinished_tail(self):
        size = live.BYTES_PER_SECOND
        pcm = b''.join(bytes([index])*size for index in range(16))
        audio = live.AudioBuffer()
        audio.data.extend(pcm)
        audio.done = True
        seen, events = [], []
        texts = iter([[self.segment('uno', 3), self.segment('dos', 7), self.segment('tres', 8)],
                      [self.segment('tres', 6), self.segment('cuatro', 8)],
                      [self.segment('cuatro', 3)]])
        def transcribe(chunk):
            seen.append(chunk)
            return next(texts)
        live.transcribe_stream(audio, transcribe, 2, 8, 1,
                               lambda event, **fields: events.append((event, fields['text'])))
        self.assertEqual(seen, [pcm[:8*size], pcm[7*size:15*size], pcm[13*size:]])
        self.assertEqual(events, [('final', 'uno dos'), ('partial', 'tres'),
                                  ('final', 'tres'), ('partial', 'cuatro'), ('final', 'cuatro')])

    def test_samples_arriving_during_inference_are_retained(self):
        size = live.BYTES_PER_SECOND
        audio = live.AudioBuffer()
        audio.data.extend(b'a' * (8*size))
        seen = []
        def transcribe(chunk):
            seen.append(chunk)
            if len(seen) == 1:
                with audio.condition:
                    audio.data.extend(b'b' * (5*size))
                    audio.done = True
            return [self.segment('texto', 6), self.segment('final', 8)]
        live.transcribe_stream(audio, transcribe, 2, 8, 1, lambda *args, **kwargs: None)
        self.assertEqual(seen, [b'a'*(8*size), b'a'*(2*size)+b'b'*(5*size)])

    def test_stopping_during_inference_reuses_the_result_if_it_covers_all_audio(self):
        audio = live.AudioBuffer()
        audio.data.extend(b'a' * live.BYTES_PER_SECOND)
        calls, events = [], []
        def transcribe(chunk):
            calls.append(chunk)
            with audio.condition:
                audio.done = True
            return [self.segment('Listo.', 1)]
        live.transcribe_stream(audio, transcribe, 0.01, 8, 1,
                               lambda event, **fields: events.append((event, fields['text'])))
        self.assertEqual(len(calls), 1)
        self.assertEqual(events, [('final', 'Listo.')])

    def test_one_long_segment_advances_instead_of_repeatedly_decoding_the_same_window(self):
        audio = live.AudioBuffer()
        audio.data.extend(b'a' * live.BYTES_PER_SECOND * 9)
        audio.done = True
        sizes = []
        def transcribe(chunk):
            sizes.append(len(chunk) / live.BYTES_PER_SECOND)
            return [self.segment('Frase larga.', sizes[-1])]
        live.transcribe_stream(audio, transcribe, 2, 8, 1, lambda *args, **kwargs: None)
        self.assertEqual(sizes, [8, 1])

    def test_silence_advances_without_creating_text_or_a_backlog(self):
        audio = live.AudioBuffer()
        audio.data.extend(b'\0' * live.BYTES_PER_SECOND * 16)
        audio.done = True
        sizes, texts = [], []
        def transcribe(chunk):
            sizes.append(len(chunk) / live.BYTES_PER_SECOND)
            return []
        live.transcribe_stream(audio, transcribe, 2, 8, 1,
                               lambda event, **fields: texts.append(fields['text']))
        self.assertEqual(sizes, [8, 8, 2])
        self.assertFalse(any(texts))

    def test_partial_text_is_emitted_before_capture_finishes(self):
        audio = live.AudioBuffer()
        audio.data.extend(b'a' * live.BYTES_PER_SECOND)
        draft_seen = threading.Event()
        events = []
        def finish_capture():
            draft_seen.wait(timeout=3)
            with audio.condition:
                audio.data.extend(b'b' * live.BYTES_PER_SECOND)
                audio.done = True
                audio.condition.notify_all()
        producer = threading.Thread(target=finish_capture)
        producer.start()
        def output(event, **fields):
            events.append(event)
            if event == 'partial': draft_seen.set()
        live.transcribe_stream(audio, lambda _: [self.segment('hola', 1)], 0.01, 8, 1, output)
        producer.join(timeout=1)
        self.assertEqual(events, ['partial', 'final'])

    def test_empty_capture_never_runs_inference(self):
        audio = live.AudioBuffer()
        audio.done = True
        live.transcribe_stream(audio, lambda _: self.fail('Empty audio was decoded'),
                               2, 8, 1, lambda *args, **kwargs: self.fail('Empty event'))


if __name__ == '__main__':
    unittest.main()
