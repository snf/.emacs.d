# Rebuilt-container STT/TTS validation

Validated on 2026-11-04 as the `core` user in the rebuilt Debian container.
The required speech pipeline works using the installed native tools and models.

The review found and corrected three configuration issues:

- Kokoro still defaulted to the old research cache. It now automatically selects
  `/opt/kokoro.cpp`; explicit paths retain priority and the cache remains a fallback.
- `setq` changed Whisper's public installation directory without updating its
  internal executable path. `setopt` now points both CLI and server at `/tmp/whisper.cpp`.
- The saved Silero override selected a research file. The local override is now
  nil, so Whisper uses the installed VAD model beside Small.

The installed Kokoro commit and its GGML/Highway submodules match the selected
versions. Both model/voice SHA-256 checks match the pinned downloads. The shared
library loads system eSpeak NG and Vulkan libraries without missing dependencies.
The runtime and models are owned by root and readable by the `core` user.

| Component | Verified location |
| --- | --- |
| Kokoro library | `/opt/kokoro.cpp/build/libkokoro.so` |
| Kokoro F16 weights | `/opt/kokoro.cpp/models/kokoro-82m-f16.gguf` |
| English voice | `/opt/kokoro.cpp/models/kokoro-voice-af_heart.gguf` |
| eSpeak data parent | `/opt/kokoro.cpp` |
| Whisper CLI/server | `/tmp/whisper.cpp/build/bin/` |
| Spanish Small model | `/tmp/whisper.cpp/models/ggml-small.bin` |
| Silero VAD | `/tmp/whisper.cpp/models/ggml-silero-v6.2.0.bin` |
| Capture/playback | System `pacat`, `paplay`, `pactl`, and FFmpeg |
| Python/WebSocket | Configured isolated uv environment; Python 3.15.2, websockets 17.4 |

Both Whisper's speech model and Kokoro use Vulkan on the AMD Radeon GPU. Native
logs and the TTS ready event confirm the backend and loaded model paths. Silero
and the runtimes' existing host utility work retain their normal CPU operations.

The recorded runtime test result was **122 passing tests, 2 intentional skips**:

| Suite | Passed | Skipped |
| --- | ---: | ---: |
| Chatty Python unit tests | 21 | 0 |
| Whisper Python regression tests | 14 | 0 |
| Emacs ERT voice/Whisper/server tests | 77 | 2 |
| Live app-server/GPU/audio integration tests | 10 | 0 |

Commit review also tested the exported staged source: **121 passed, 2 skipped**.
That snapshot excludes the separate, uncommitted proxy logging regression test.

At the user's request, `whisper-file-better` is commented out, with its full
implementation retained. It is also undefined in the running Emacs. Its two
tests skip until the optional command is enabled again. Its Large v3 model is
not required by Chatty; Small and ordinary file/realtime commands remain active.

Two additional real checks passed:

1. Emacs started and stopped a recording through `codex-chatty`; private Pulse
   input carried a natural Spanish fixture through GPU Whisper, Terra, GPU
   Kokoro and actual playback drain. This used the real configured Python command
   and installed model paths, and no handoff occurred.
2. The container's GPU Whisper CLI transcribed the recorded English TTS output.
   It recovered coherent sentences, with 82.5% exact word recovery; technical
   names such as `af_heart` were transcribed phonetically.

Two consecutive controller recordings reused the same Whisper and Kokoro workers.
Both Spanish transcriptions retained “No envíes ningún mensaje.” The output file
preserves real startup waits and playback timing:
[full-pipeline-output.wav](full-pipeline-output.wav).

| Trial | Stop to final transcript | Stop to first PCM |
| --- | --- | --- |
| First recording | 2.751 s | 9.552 s |
| Second recording, same workers | 2.037 s | 5.455 s |

These include STT and LLM time and are not calibrated first audible word times
at the physical speaker. They are two observations, not a latency distribution.

Audio tests used private Pulse null sinks and muted default-device playback.
The configured physical microphone is present and unmuted; its audio was not
recorded. All target Codex threads were disposable and read-only. The tests
released their audio modules and model workers on completion.

See [validation-summary.json](validation-summary.json),
[emacs-audio-transcript.txt](emacs-audio-transcript.txt),
[emacs-audio-events.json](emacs-audio-events.json),
[full-pipeline-events.json](full-pipeline-events.json),
[tts-roundtrip.txt](tts-roundtrip.txt), and the suite logs for evidence.
Recordings, transcripts, and detailed logs are local generated artifacts.
