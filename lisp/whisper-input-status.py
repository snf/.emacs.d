# /// script
# dependencies = ["pulsectl"]
# ///
"""Read microphone state without recording audio or changing device settings."""
import argparse
import json
import os
import time

import pulsectl


def input_status(pulse, requested, recording_pid, original):
    sources = {source.index: source for source in pulse.source_list()}
    selected = pulse.server_info().default_source_name if requested == 'default' else requested
    source = next((item for item in sources.values() if item.name == selected), None)
    for stream in pulse.source_output_list():
        if stream.proplist.get('application.process.id') == str(recording_pid):
            source = sources.get(stream.source)
            break
    result = {'source': source.name if source else None,
              'description': source.description if source else None}
    if original and not any(item.name == original for item in sources.values()):
        return dict(result, warning='Microphone disconnected — reconnect it or select another input'), original
    if source is None:
        return dict(result, warning='Microphone disconnected — no recording input available'), original
    original = original or source.name
    port = source.port_active
    if port and port.available == pulsectl.PulsePortAvailableEnum.no:
        warning = f'Microphone disconnected — {source.description}'
    elif source.mute:
        warning = f'Microphone muted — {source.description}'
    elif not any(source.volume.values):
        warning = f'Microphone volume is zero — {source.description}'
    elif source.monitor_of_sink != 4294967295:
        warning = 'Selected input is a speaker monitor — select a microphone'
    else:
        warning = None
    return dict(result, warning=warning), original


def recording_alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', default='default')
    parser.add_argument('--recording-pid', type=int, default=0)
    parser.add_argument('--expected-source')
    parser.add_argument('--once', action='store_true')
    args = parser.parse_args()
    original = args.expected_source or (None if args.source == 'default' else args.source)
    while args.once or recording_alive(args.recording_pid):
        try:
            with pulsectl.Pulse('emacs-whisper-input-status') as pulse:
                while args.once or recording_alive(args.recording_pid):
                    status, original = input_status(pulse, args.source, args.recording_pid, original)
                    print(json.dumps(status, ensure_ascii=False), flush=True)
                    if args.once:
                        return
                    time.sleep(2)
        except pulsectl.PulseError:
            print(json.dumps({'warning': 'Microphone status unavailable — check the audio server'}), flush=True)
            if args.once:
                return
            time.sleep(2)


if __name__ == '__main__':
    main()
