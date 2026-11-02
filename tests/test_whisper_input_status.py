"""Read-only PulseAudio status tests; no microphone or audio server needed."""
import importlib.util
from pathlib import Path
from types import SimpleNamespace
import unittest

import pulsectl

spec = importlib.util.spec_from_file_location(
    'whisper_input_status', Path(__file__).resolve().parents[1] / 'lisp/whisper-input-status.py')
status = importlib.util.module_from_spec(spec)
spec.loader.exec_module(status)


def source(name='usb-microphone', index=1, mute=False, volume=1.0,
           port=pulsectl.PulsePortAvailableEnum.unknown, monitor=False):
    return SimpleNamespace(name=name, index=index, description=name, mute=mute,
                           volume=SimpleNamespace(values=[volume]),
                           port_active=SimpleNamespace(available=port),
                           monitor_of_sink=0 if monitor else 4294967295,
                           state='suspended')


class Pulse:
    def __init__(self, sources, default='usb-microphone', streams=()):
        self.sources, self.default, self.streams = sources, default, streams

    def source_list(self): return self.sources
    def source_output_list(self): return self.streams
    def server_info(self): return SimpleNamespace(default_source_name=self.default)


class MicrophoneStatusTests(unittest.TestCase):
    def test_idle_connected_microphone_is_not_a_disconnect(self):
        result, original = status.input_status(Pulse([source()]), 'default', 123, None)
        self.assertIsNone(result['warning'])
        self.assertEqual(original, 'usb-microphone')

    def test_muted_and_zero_volume_inputs_warn(self):
        for microphone, expected in [(source(mute=True), 'muted'), (source(volume=0), 'volume is zero')]:
            with self.subTest(expected=expected):
                result, _ = status.input_status(Pulse([microphone]), 'default', 123, None)
                self.assertIn(expected, result['warning'])

    def test_disconnect_is_reported_even_after_automatic_fallback(self):
        fallback = source('internal-microphone')
        result, _ = status.input_status(Pulse([fallback], fallback.name), 'default', 123, 'usb-microphone')
        self.assertIn('disconnected', result['warning'])

    def test_preflight_catches_a_disconnect_between_recordings(self):
        fallback = source('internal-microphone')
        result, remembered = status.input_status(Pulse([fallback], fallback.name),
                                                 'default', 0, 'usb-microphone')
        self.assertIn('disconnected', result['warning'])
        self.assertEqual(result['source'], 'internal-microphone')
        self.assertEqual(remembered, 'usb-microphone')

    def test_reconnecting_original_microphone_clears_warning(self):
        result, _ = status.input_status(Pulse([source()]), 'default', 123, 'usb-microphone')
        self.assertIsNone(result['warning'])

    def test_actual_recording_source_wins_over_a_changed_default(self):
        microphone, fallback = source(), source('internal', index=2, mute=True)
        stream = SimpleNamespace(source=1, proplist={'application.process.id': '123'})
        result, _ = status.input_status(Pulse([microphone, fallback], 'internal', [stream]),
                                        'default', 123, 'usb-microphone')
        self.assertIsNone(result['warning'])

    def test_missing_source_unavailable_port_and_monitor_are_not_healthy(self):
        cases = [(Pulse([]), 'disconnected'),
                 (Pulse([source(port=pulsectl.PulsePortAvailableEnum.no)]), 'disconnected'),
                 (Pulse([source(monitor=True)]), 'speaker monitor')]
        for pulse, expected in cases:
            with self.subTest(expected=expected):
                result, _ = status.input_status(pulse, 'default', 123, None)
                self.assertIn(expected, result['warning'])


if __name__ == '__main__':
    unittest.main()
