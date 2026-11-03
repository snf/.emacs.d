# Chatty Codex

Open a spoken discussion alongside a working Codex terminal. Emacs handles the
UI; one Python controller owns the session and its audio/model workers.

## Use

From the intended working Codex Ghostel buffer, press **C-c C-v** or run
`M-x codex-chatty`. Recording starts while Whisper, Terra, and Kokoro prepare in
parallel. The transcript header changes to **Listening** when PCM capture starts.
Press the same command again to stop recording and submit the complete Spanish
utterance to Chatty. Chatty answers in concise English through Kokoro.
The prompt asks for spoken descriptions of sources and files; exact URLs and
paths belong in tool calls and working-Codex instructions.

Use **C-c C-c** inside the transcript to record/stop. Starting a new recording
during an answer stops its playback and interrupts the old Chatty turn.
**C-c C-k**, **q**, or `M-x codex-chatty-close` closes the discussion. Killing the
transcript or original terminal also closes it. There is one active channel;
switching buffers cannot change its bound working thread.

Discussion, file inspection, and draft-only requests stay in Chatty. A clear
instruction such as “Envíale a Codex que implemente lo que acordamos” delegates
the composed English request. On server acceptance, Chatty says “Sent it to
Codex,” drains playback, and closes. Working Codex keeps running. Open Chatty
again for a fresh session with the latest saved context. Ordinary `C-c C-w`
voice follow-ups retain their existing behavior.

When working Codex is busy, delegation steers its active turn. Explicitly asking
to wait uses a pending local handoff: Chatty stays open until the active turn
ends and the server accepts the new instruction. Closing before delivery
cancels that pending handoff. An unconfirmed submission is never retried
automatically; inspect the working thread before asking to resend.

## Runtime

For installation in a rebuilt development container, use the
[pinned Kokoro Docker recipe and Emacs path settings](container-install.md).

The existing Emacs-managed Codex app-server is reused. Chatty creates a separate
ephemeral `gpt-6.1-terra` thread with `medium` effort, a read-only sandbox, native
web search, bounded project read/search tools, and the fixed-target handoff tool.
Inherited apps, plugins, MCP servers, shell execution, hooks, goals, memory, and
project instruction discovery are disabled for this thread. The shared server's
configuration is not changed.

Initial context comes from `codex-voice--read-context`: the last saved user and
assistant exchange, capped at 16,384 characters per field, with its existing
terminal fallback. Chatty retains its own history until closure.

Python needs `websockets` (tested with 17.0.1). The default command reuses
`codex-app-server-python-command`, currently `uv run --project /projects/takopi
python`; there is no second service to start. Override
`codex-chatty-python-command` with another Python command prefix if needed.

Whisper reuses the configured local whisper.cpp server, Spanish small model,
Silero VAD, and microphone selection. Its backend stays warm across recordings.
Chatty decodes incrementally every two seconds with a 30-second window and
three-second overlap. Only manual stop authorizes an utterance; unexpected
capture EOF leaves an unsent draft. Original dictation settings are unaffected.

On this host, capture and playback use `pacat` through
`PULSE_SERVER=unix:/run/host-pulse/native`. Pulse capture is 16 kHz mono PCM;
playback is 24 kHz mono PCM16 with a requested 40 ms latency. An installed
`pacat` or the cached native client is selected automatically. FFmpeg remains
the input fallback and supports file/non-Pulse input. Output falls back to
FFmpeg Pulse or `pw-cat`, depending on the environment. The standalone PipeWire
client cannot reach an audio daemon in the current environment.

Kokoro uses the existing research cache under
`~/.cache/codex-chatty-tts/`:

| Asset | Relative path |
| --- | --- |
| Pinned native library | `kokoro.cpp/build/libkokoro.so` |
| F16 model | `models/kokoro-cpp/kokoro-82m-f16.gguf` |
| English voice | `models/kokoro-cpp/kokoro-voice-af_heart.gguf` |
| Native dependencies and phonemizer data | `deps/root/usr/lib/x86_64-linux-gnu/` |
| Cached Pulse tools | `deps/root/usr/bin/` |

The runtime is `simonfxr/kokoro.cpp` at
`a9e31430838c8bdc3d4beaaab62753681b9f7839`. It explicitly selects Vulkan device
0 and sets `GGML_VK_ALLOW_GRAPHICS_QUEUE=1` before loading. Weighted neural
operations run on the GPU; phonemization, utility operations, and waveform
reconstruction retain the runtime's documented CPU work. There is no CPU-model
fallback. Speaking rate is the approved native `length_scale=1/1.526`; audio
is not stretched or cut to a duration.

One child process silently warms and retains Kokoro for the whole session.
The first complete useful sentence can synthesize immediately; subsequent
chunks are bounded to 240 characters. Synthesis and PCM playback run separately
with a bounded playback queue. Interruption discards stale synthesis results
while keeping the GPU model loaded. Closing releases all session workers.

`codex-chatty-tts-options` accepts an alist for `library`, `model`, `voice`,
`device`, `threads`, `speed`, or `player_command`. Defaults match the approved
configuration. `codex-chatty-record-on-open` can be nil for opening without
recording, chiefly useful in automated tests.

## Diagnostics and validation

The transcript displays errors and the composed handoff. `*Chatty Log*` captures
controller diagnostics. Private session logs and handoff receipts live under
`~/.local/state/codex-chatty/` (files mode 0600). Timing events include capture
readiness, transcript completion, first PCM, chunk generation, and playback
drain. First PCM is not a measurement of the first audible word at the speaker.
After an accepted handoff, playback errors leave the receipt intact.

Run the controller tests with the configured Python environment:

```sh
/projects/takopi/.venv/bin/python -m unittest discover -s tests -p test_codex_chatty.py -v
CODEX_CHATTY_LIVE=1 PYTHONPATH=tests /projects/takopi/.venv/bin/python -m unittest test_codex_chatty_live -v
```

The opt-in tests require the running Emacs/app-server, GPU/cache, local Whisper
models, and host PulseAudio. They use disposable read-only target threads,
private null sinks for Spanish fixture capture/output, and muted default-device
playback. They exercise the real Emacs adapter, routing, file reads, busy
steering/waiting, acceptance and closure, worker reuse, audio drain and
interruption, and failure reporting. No test submits to a real working thread.

ERT coverage is in `tests/codex-chatty-tests.el`; run it with the normal package
load paths. Existing voice/Whisper regression suites should run in separate
Emacs processes with Whisper modules loaded first, matching the live setup.
This avoids their existing cross-suite global/advice-state interference.

The full output capture, Spanish fixture, transcript, and timing events are in
[`tts-samples/2026-11-03/implementation-validation/`](../../tts-samples/2026-11-03/implementation-validation/).
The report and summary are versioned; recordings and logs stay local. These
captures preserve silence while waiting for STT/LLM/TTS and actual playback timing.
The only remaining subjective check is the user's microphone and listening
experience.
