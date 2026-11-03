# Chatty implementation validation

Only this report and `validation-summary.json` are versioned. Audio recordings,
input fixtures, event captures, and detailed logs remain local generated
artifacts; the links to those files below work where the tests were run.

All **121 checks passed** on 2026-11-03:

| Suite | Checks |
| --- | --- |
| Chatty Python unit tests | 19 |
| Existing Whisper Python regressions | 14 |
| Emacs ERT: Chatty, voice, realtime, microphone, target, app-server | 78 |
| Real GPU, Whisper, Terra, web, audio and Emacs integration | 10 |

The real tests create disposable read-only working threads. They never send
test messages to the user's working conversation. Audio tests use private
PulseAudio input/output sinks and muted default-device output. Those sinks and
model/audio workers are removed at test completion.

Listen to [full-pipeline-output.wav](full-pipeline-output.wav). It contains two
English answers with their real startup waits and playback timing, recorded
from the private output monitor. [spanish-input.wav](spanish-input.wav) is the
natural Spanish Kokoro `ef_dora` test fixture; the production English voice is
the selected `af_heart`. The fixture says, “No envíes ningún mensaje. Solo
quiero que expliques la última respuesta en inglés.” Both captures retained
the negation. Whisper and Kokoro worker PIDs stayed the same across recordings.

| Trial | Stop to final transcript | Stop to first PCM |
| --- | --- | --- |
| First utterance | 2.788 s | 6.316 s |
| Second utterance, same workers | 1.968 s | 3.937 s |

These are two complete-pipeline observations, including STT and LLM work.
First PCM is not calibrated first audible speech at the physical speaker.
The private input monitor took about 2.085 seconds to begin producing PCM
from a cold null sink. A focused warm Pulse-client check took 0.12–0.16 seconds;
this does not establish physical microphone startup time.

[validation-summary.json](validation-summary.json) records counts and timings.
[full-pipeline-events.json](full-pipeline-events.json) contains the event
timeline. [emacs-transcript.txt](emacs-transcript.txt) and
[emacs-live-test.log](emacs-live-test.log) show the real Emacs adapter completing
a request through the production controller and GPU speech worker. The other
logs retain each passing suite's output. Existing modules emit some Emacs 31
obsolete-macro warnings; they did not fail validation.

For the existing Python microphone-status tests, use their `pulsectl`
dependency (`uv run --no-project --with pulsectl python ...`). The Chatty
controller itself only adds `websockets`, already in the Takopi environment.
Existing ERT suites run in separate processes with Whisper modules preloaded,
matching the live setup and avoiding their existing cross-suite global state.
