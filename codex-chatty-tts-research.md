# Local GPU TTS Research for Chatty Codex

Date: 2026-11-03

The first models to evaluate are **Soprano 1.1 80M** and **Kokoro 82M**, using
native Vulkan implementations. **Phonon 31M** and **Supertonic 3** are additional
lightweight candidates with explicit WebGPU implementations. English output,
intelligibility, reasonable naturalness, and short pauses matter more than
high-fidelity audio or voice cloning.

This catalog groups released checkpoints into model families. It distinguishes
documented GPU implementations from experimental ports; neither is a claim that
the model has passed inference and listening tests on this exact machine.

Follow-up: the first four models have now produced samples on this machine using
audio.cpp 0.9.3 with explicit Radeon Vulkan selection. Their neural weight buffers
and per-process GPU compute activity were checked. Open the
[30-second comparison player](tts-samples/2026-11-03/index.html), or read the
[measured results and method](tts-samples/2026-11-03/README.md). Each model rendered
the same 99-word English passage after a separate short warmup:

| Model | Measured full render | Generated speech |
| --- | ---: | ---: |
| Soprano 1.1 80M | 14.03 s | 30.14 s |
| Kokoro 82M | 28.35 s | 39.05 s |
| Phonon 31M | 9.18 s | 34.08 s |
| Supertonic 3 | 8.62 s | 41.37 s |

The listening copies are cropped to exactly 30 seconds; complete recordings are
also saved. These times measure full offline rendering, not streaming first-audio
latency. Automatic transcription recovered coherent English from all four;
naturalness remains for listening comparison. Other catalog entries are still
research candidates rather than on-device benchmark results.

Native speaking-rate follow-up: the requested duration applies to the complete
unchanged 99-word passage, not an excerpt cut from a longer waveform. Kokoro and
Supertonic were regenerated and their native rates calibrated to fit within about
25 seconds, preserving text, voice, seed, and other settings:

| Model | Native speed | Complete generated duration | Measured generation time |
| --- | ---: | ---: | ---: |
| Kokoro 82M | 1.526 | 24.75 s | 18.39 s |
| Supertonic 3 | 1.669 | 24.79 s | 5.72 s |

The player now uses complete recordings for every model, including these faster
ones. No cropping, silence padding, or waveform speed processing was applied to
the new recordings. GPU execution was verified again. Earlier speed-1.2 renders
and their cropped excerpts remain as prior measurements but did not satisfy the
full-passage target. Calibration trials and selected full recordings are under
`tts-samples/2026-11-03/native-target-25s/`. The listed times are for the final
selected renders and exclude earlier calibration attempts.

The faster Kokoro transcript has about 4% normalized word mismatch against the
input (1% at baseline). Supertonic's rises to about 26% (3% at baseline), missing
or misrecognizing several phrases. A 16-step trial did not resolve that concern.
These automatic figures include recognizer errors, but Supertonic needs careful
listening at the target rate. The player flags this and exposes the transcripts;
the complete generated output is preserved even where pronunciation is unclear.

Kokoro and Supertonic apply this rate before generating speech. Soprano exposes
no native rate option in this runtime. Phonon supports upstream `duration_delta`,
but this audio.cpp port folds a fixed zero condition into its loaded weights and
exposes no setting for it. Faster Phonon delivery would need another GPU runtime
or an implementation change. See the [native-rate method and source links](tts-samples/2026-11-03/README.md#native-speaking-rate-comparison).

## Kokoro latency optimization after listening

The user preferred Kokoro at the faster speaking rate. Further local experiments
found a substantially better configuration using the standalone
`simonfxr/kokoro.cpp` runtime, F16 weights, the same af_heart voice and native
`length_scale = 1 / 1.526`, explicit Vulkan device 0, and a persistent context.
The launcher must set `GGML_VK_ALLOW_GRAPHICS_QUEUE=1` before Vulkan initializes.
Send a short complete first sentence, then larger chunks ending at sentence
boundaries. Uniformly tiny chunks reduced pronunciation quality in the automatic
transcript; the first-sentence strategy retained a close match.

| TTS metric | Recommended measured configuration |
| --- | ---: |
| First playable audio | 0.558 s |
| Estimated audible speech onset, including leading quiet audio | 0.788 s |
| Complete generation | 6.03 s |
| Complete generated speech duration | 24.35 s |
| Predicted playback underflow | 0 s |

This is about three times faster to generate the complete passage than the
earlier 18.39-second offline result. These are separate single-run observations,
not medians. They exclude STT, Terra generation, model loading, short warmup,
and sound-device buffering. The initial 99-word text is unchanged and the full
generated waveform is saved without cropping or waveform rate processing.
The local Whisper check differs from the reference by two words out of 99,
including recognition errors; listening is still the quality check.

The physical Radeon GPU performed the work on its **graphics** engine in the
selected mode. A complete-passage scheduler diagnostic assigned all 5,601 matrix
multiplication nodes to Vulkan0 and no weighted neural operation to CPU. Host
alignment/gather, copies, reflection padding and a small LeakyReLU step remain;
phonemization, harmonic source and iSTFT also use CPU. This is not a claim of
literally every operation running on the GPU.

For comparison, the original audio.cpp runtime started audio in 7.92 seconds
with default chunks and 1.84 seconds with 80-character chunks. Its full-F16 and
convolution-only F16 paths failed on an unsupported Vulkan REPEAT operation;
tiered graph sizing was slower here. Selecting the graphics-capable queue in
that runtime improved the 80-character variant to 1.22 seconds first audio and
14.52 seconds total generation. The dedicated runtime and sentence strategy
provided the best tested combination.

Open the [latency player and full samples](tts-samples/2026-11-03/kokoro-optimization/index.html)
and [measurement details, GPU evidence and reproduction](tts-samples/2026-11-03/kokoro-optimization/README.md).
Source: [pinned kokoro.cpp GPU implementation and queue documentation](https://github.com/simonfxr/kokoro.cpp/blob/a9e31430838c8bdc3d4beaaab62753681b9f7839/GPU_OPTIMIZATION.md).

Follow-up listening evidence: [fresh recordings of the clocked PCM stream](tts-samples/2026-11-03/live-stream-recordings/index.html)
include startup wait and actual chunk underflow. Warm first playback is about
0.52–0.56 seconds (estimated audible onset 0.75–0.81 seconds). A fresh-worker
example includes loading and silent warmup, with first playback at 1.95 seconds.
The new conversational example retains a 0.48-second gap after its first sentence;
the previous zero-underflow observation applies to that passage and arrival
schedule, not all responses. These captures use no physical sound-device loopback
and exclude STT/LLM delays. Individual raw chunks and exact frame positions are
saved beside the complete captured WAVs.

## Hardware actually available

| Item | Observed value |
| --- | --- |
| Processor | AMD Ryzen 7 5825U with Radeon Graphics |
| GPU | Integrated Radeon Vega, `gfx90c`; PCI `1002:15e7` |
| Vulkan device | `AMD Radeon Graphics (RADV RENOIR)` |
| Driver | Mesa RADV 25.0.7; Vulkan device API 1.4.305 |
| Reserved VRAM reported by sysfs | 1 GiB |
| Shared graphics memory reported by sysfs | About 19 GiB GTT |
| Installed speech.cpp device enumeration | `Vulkan0`, integrated GPU, 21.5 GB total and 17.7 GB free at inspection |
| System memory | About 38 GiB total; roughly 12 GiB available at inspection; no swap |
| GPU access | `/dev/dri/renderD128` accessible; `/dev/kfd` absent in this environment |
| ROCm tooling | No `rocminfo`, `rocm-smi`, or `/opt/rocm` installation found |
| Software Vulkan device | `llvmpipe` also exists; it is CPU rendering and must not be selected |

The iGPU shares system RAM. The 1 GiB reserved allocation is not a hard 1 GiB
limit on model weights, and Vulkan's memory budgets are not additional physical
RAM. We still need to measure working memory with Whisper, Emacs, and desktop
applications resident. The existing runtime reports GPU availability through:

```text
/projects/transcribe/.local-asr-research/speech.cpp/speech devices
```

Use **Vulkan or WebGPU** for the initial evaluation. A model advertising CUDA does
not thereby support this AMD GPU. A PyTorch ROCm implementation would additionally
need device exposure, a suitable runtime, and working kernels for `gfx90c`.
AMD's current TheRock table marks `gfx90c` build-passing, with no sanity-tested or
release-ready mark. That is an experimental possibility, not proof of a usable
ROCm environment here. [AMD GPU support status](https://github.com/ROCm/TheRock/blob/main/SUPPORTED_GPUS.md).

CPU text normalization, control logic, and audio playback are expected. The GPU
requirement concerns neural synthesis: do not count a CPU TTS engine, a software
Vulkan adapter, or a substantial neural decoder silently placed on the CPU as
meeting the requirement. Any remaining neural CPU stages must be disclosed and
evaluated separately.

## Models with explicit GPU implementation evidence

These are evaluation candidates, not a completed compatibility matrix. Model
parameter counts and downloaded file sizes do not include all runtime buffers,
codecs, graph scratch, or caches.

| Model family and variants | GPU implementation to evaluate | Evidence and implications for Chatty |
| --- | --- | --- |
| **Soprano 1.1 80M** | `audio.cpp`, Vulkan | Its port documents Vulkan synthesis on an AMD Radeon RX Vega 8 GB card. It supports chunked audio output and English speech. This is the closest published GPU architecture match found, but the discrete Vega card is faster than this APU. |
| **Kokoro 82M** | `simonfxr/kokoro.cpp`, Vulkan; alternatively `kokoro-js`, WebGPU | The standalone native port documents AMD Vulkan synthesis. F16 weights are about 156 MiB and its Q8 package about 135 MiB, plus a voice pack. Native synthesis returns a full utterance, so feed short phrases for conversational playback. |
| **Phonon 31M English** | Gradium `phonon-tts`, explicit WebGPU mode | The publisher's browser implementation runs the model on WebGPU and yields audio chunks. Public weights and a small model make it a useful latency candidate; browser integration is extra work for Emacs. |
| **Phonon 40M English and multilingual** | Same publisher WebGPU implementation, with locally served weights | Similar route, but the 40M downloads are gated. Do not assume the default Python package or server is using a GPU. |
| **Supertonic 1 and 2, about 66M; Supertonic 3, about 99M** | Official ONNX WebGPU examples; native Vulkan ports also exist | Fixed voices and compact synthesis. The publisher's open-source repositories and weights are archived, so use the archive locations and account for maintenance. |
| **Qwen3-TTS 0.6B Base and CustomVoice** | `nyosegawa/speech.cpp`, Vulkan; `audio.cpp` and Vox are alternatives | Native GPU route and streaming audio. speech.cpp lists Q8, Q6, Q5, and Q4 conversions. Its Q5 CustomVoice package is about 0.87 GB; this is a file size, not a measured memory requirement here. |
| **Qwen3-TTS 1.7B Base, CustomVoice, VoiceDesign** | Same family of Vulkan runtimes, with checkpoint support checked per runtime | More model work and memory than the 0.6B variants. The speech.cpp CustomVoice Q5 package is about 1.57 GB. Quality may justify it, but latency on this APU needs measurement. |
| **F5-TTS v0 and v1** | Vox native Vulkan; other native ports exist | Vox documents a Vulkan DiT path. More integration and reference-voice setup than fixed-voice engines. Check where the Vocos decoder runs, not only the DiT. |
| **CosyVoice2 0.5B** | Vox native Vulkan | Its model matrix explicitly covers the token LM and flow estimator on Vulkan. Check vocoder placement before calling the full pipeline GPU-backed. |
| **Chatterbox original 0.5B** | Vox native Vulkan; alternative native ports | Vox documents GPU T3 and flow-estimator execution. Audit the remaining synthesis stages and compare first-audio latency with the smaller models. |
| **VoxCPM1 0.5B family** | `audio.cpp`, Vulkan | The port explicitly describes stateful streamed AudioVAE decoding on Vulkan. Its published memory figures are not measurements of this APU. |
| **VoxCPM2 2B** | `voxcpm-rs`, wgpu/Vulkan; native ggml ports | A dedicated Rust implementation documents GPU execution. Its bf16-native route must not be assumed compatible with this older Vega GPU; select an appropriate supported precision and verify it. |
| **Ming 3.0 TTS 0.5B** | `audio.cpp`, Vulkan | The model-specific documentation explicitly lists Vulkan inference and streaming. Still requires an exact-device smoke test. |
| **Piper VITS English voices** | `piper-tts-web`, ONNX WebGPU; native ggml alternatives | A WebGPU implementation exists. Voice export and graph support are checkpoint-specific. The native audio.cpp port describes GPU duration issues and possible host placement, so it is conditional under a strict neural-GPU requirement. |
| **Pocket TTS 100M; newer 18M, 7M, and 2M English checkpoints** | Portable RLX implementations; native ggml ports for supported checkpoints | The default publisher server runs on CPU and does not qualify. Portable GPU paths exist, but checkpoint coverage and full codec placement need checking. A native runtime documents a long-output Vulkan guard that falls back to CPU Mimi decoding. |

Primary references for this table:

- [Soprano Vulkan implementation and Vega measurements](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/soprano_tts.md).
- [Kokoro native GPU runtime](https://github.com/simonfxr/kokoro.cpp), [publisher's WebGPU and text-stream examples](https://github.com/hexgrad/kokoro/blob/main/kokoro.js/README.md).
- [Phonon models and entry points](https://github.com/gradium-ai/xn-ptts), [explicit WebGPU selection and failure behavior](https://github.com/gradium-ai/xn-ptts/blob/main/ptts-wasm/js/README.md).
- [Supertonic publisher archive and versions](https://github.com/supertone-oss-archive/supertonic).
- [Qwen3-TTS native model variants and memory measurements on other hardware](https://github.com/nyosegawa/speech.cpp/blob/main/docs/models/qwen3-tts.md), [publisher checkpoints](https://github.com/QwenLM/Qwen3-TTS).
- [Vox per-model GPU matrix](https://github.com/dariumi/Vox).
- [VoxCPM1 streaming implementation](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/voxcpm1.md), [VoxCPM2 Rust GPU implementation](https://github.com/mii-nipah/voxcpm-rs).
- [Ming 3.0 TTS runtime](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/ming3_tts.md).
- [Piper WebGPU runtime](https://github.com/Poket-Jony/piper-tts-web), [native Piper placement caveats](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/piper_tts.md).
- [Pocket publisher runtime behavior](https://github.com/kyutai-labs/pocket-tts), [portable model coverage](https://github.com/MIT-RLX/rlx-models/blob/main/MODELS.md), [native placement controls](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md#pocket-tts-languages-voices-and-environment-switches).

## Additional portable GPU candidates

These families have native or WebGPU-capable ports worth retaining in the
inventory. Evidence is weaker than a tested, complete synthesis path on Vega.
Some implementations mix GPU neural work with CPU neural stages, and some only
advertise a backend without validating that model on it. They are **not confirmed
matches** to the strict GPU requirement until placement and output are checked.

### Compact and conventional models

| Model | Candidate implementation or unresolved point |
| --- | --- |
| KittenTTS 0.8 Mini 80M, Micro 40M, Nano 15M | RLX GPU ports; audio.cpp documents the 80M conversion. Do not infer that all sizes work from the Mini conversion alone. |
| KittenTTS 2, 1.7B | audio.cpp and RLX ports; LM and S3Gen GPU coverage differ. The published CUDA reference requires much more memory than the old ONNX models. |
| sanoTTS voice family, about 0.3M–2.27M | kokopop Vulkan and portable ports. GPU verification and listening quality remain to be established. |
| Inflect-Nano v1, about 4.6M | RLX Vulkan path. Keep separate from Inflect v2. |
| Inflect Micro v2 and Nano v2 | audio.cpp port; its documentation explicitly says Linux Vulkan is not practically validated. |
| MeloTTS and TinyTTS VITS2 derivatives | RLX/CrispASR ports. Check encoder and vocoder placement. |
| SpeechT5 | CrispASR native port; exact-device GPU coverage needs checking. |
| FastPitch with its vocoder | CrispASR native port; qualify the complete pair. |
| BananaMind TTS V2.1, 13M | CrispASR native port; compact, but no Vega inference result established. |

Sources: [Kitten Mini conversion](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/kitten_tts.md),
[KittenTTS 2 implementation](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/kitten_tts2.md),
[sanoTTS runtime](https://github.com/tterrasson/kokopop),
[Inflect v2 limitations](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/inflect_v2.md),
[RLX coverage and caveats](https://github.com/MIT-RLX/rlx-models/blob/main/MODELS.md),
[CrispASR TTS catalog](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md).

### Other speech generators

These are model-family candidates from native runtime catalogs. A catalog entry
is a lead for a GPU test, not certification that this APU can run every size,
quantization, voice-cloning component, or backend configuration.

| Model family or released variants | Candidate runtime and specific qualification |
| --- | --- |
| Chatterbox Turbo 350M | audio.cpp community port; model-specific GPU test coverage is explicitly incomplete. |
| Chatterbox Nano 110M; Multilingual variants | Publisher GPU implementations exist, but the Nano checkpoint must not be assumed compatible with a Turbo native converter. ROCm alone is not currently available here. |
| VibeVoice Realtime 0.5B and base 1.5B/7B | vibevoice.cpp, CrispASR, audio.cpp. Start with 0.5B; check VAE placement and memory for larger variants. |
| CosyVoice3 0.5B | audio.cpp/CrispASR; verify the selected Vulkan implementation and codec path. |
| MOSS TTS Nano 100M | Native ports. Quantized GPU kernel differences are documented; choose precision deliberately. |
| MioTTS 0.6B/1.7B | CrispASR/audio.cpp/RLX; model and codec variants must match. |
| OuteTTS 0.2/0.3/1.0 families | llama.cpp-backed and native ports. Verify audio decoding stays on GPU, not only the language model. |
| Orpheus 3B | Native ports exist; some portable paths run the autoregressive LM on CPU. Check both LM and SNAC. |
| Parler TTS Mini, about 878M | RLX/CrispASR. Do not extrapolate Mini support to every Large checkpoint. |
| Zonos v0.1 Transformer | Native ports; do not assume the Hybrid/Mamba version uses the same supported graph. |
| Sesame CSM 1B | Native ports; qualify the depth decoder and Mimi as well as the backbone. |
| Hume TADA 1B and 3B multilingual | RLX/CrispASR; larger than needed for fixed-voice chat. |
| Maya1 3B | audio.cpp/RLX; requires its speech codec. |
| LuxTTS | RLX portable flow-matching port; verify the full GPU path. |
| NeuTTS Air/Nano/2E | Version-dependent native ports. Match backbone and decoder. |
| Audio8 TTS / ArkTTS 0.6B | Native ports. audio.cpp explicitly says its GPU kernels have not yet been exercised for this family. |
| OmniVoice | audio.cpp/CrispASR; tokenizer placement has explicit split CPU/GPU options. |
| MiraTTS | Experimental native ports. |
| Echo TTS | audio.cpp community port. |
| Sopro V2 Turbo 120M | audio.cpp community port; published validation is chiefly CPU. |
| ZipVoice | Native flow-matching ports; backend coverage differs. |
| GLM TTS / GLM-4-Voice | Native ports, with differing completeness; do not count a CPU config-only port as GPU support. |
| Breeze TTS 2 | audio.cpp/CrispASR. |
| Confucius4 TTS | Native ports; some alternatives only implement a CPU planner. |
| dots.tts SOAR/MeanFlow/Edit | audio.cpp/CrispASR. |
| DramaBox | Native port; full Vulkan coverage not established. |
| FireRedTTS3 | audio.cpp/CrispASR. |
| Higgs Audio v3 TTS 4B | audio.cpp; separate from older Higgs v2 implementations. |
| IndexTTS 1.5, 2, and 2.5 | Runtime-dependent native ports; reference encoders and vocoders add memory. |
| Fish Audio S2 Pro | audio.cpp; quantized packages exist, but codec and graph memory still matter. |
| Voxtral TTS 4B | Native ports; substantial extra work for ordinary English acknowledgements. |
| MOSS TTS Local 4B, v1.5 8B, and TTSD | Native ports; memory-heavy and poor first choices for this APU's latency budget. |
| VieNeu TTS v3 Turbo | audio.cpp community port, with English support. |
| Gepard, about 556M | RLX; documented portable paths can leave AR generation on CPU. Not qualified as full-GPU synthesis. |
| LFM2.5 Audio and LFM3 Audio 2B | Native TTS-capable multimodal ports; preserve Chatty's text verbatim rather than introducing another conversational model. |
| Mini-Omni2 | Native speech-capable port; qualify it as literal TTS before considering integration. |
| OpenVoice v2 with MeloTTS | A composite synthesis/conversion route, not a standalone TTS checkpoint. Both components need GPU qualification. |
| Suno Bark | A native port exists; multi-stage generation and conversational latency need evaluation. |

Primary runtime references: [audio.cpp core and community catalogs](https://github.com/0xShug0/audio.cpp),
[CrispASR TTS implementations](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md),
[RLX model coverage](https://github.com/MIT-RLX/rlx-models/blob/main/MODELS.md),
[RLX actual parity results and host fallbacks](https://github.com/MIT-RLX/rlx-models/blob/main/TTS.md),
[VibeVoice native runtime](https://github.com/localai-org/vibevoice.cpp),
[Chatterbox publisher variants](https://github.com/resemble-ai/chatterbox).

Model-specific qualifications above come from the port documentation, including
[Chatterbox Turbo](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/chatterbox_turbo.md),
[Audio8 GPU coverage](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/audio8_tts.md),
[MOSS Nano precision caveats](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/moss_tts_nano.md),
[Sopro validation](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/sopro_tts.md),
and [OmniVoice component options](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md).

## Models or configurations outside the usable shortlist

- Default CPU-only packages, WASM-only browser execution, software Vulkan, and
  CUDA-only binaries do not satisfy this machine's GPU requirement. This does
  not exclude the same model through a genuine Vulkan or WebGPU implementation.
- `mmwillet/TTS.cpp` currently lists no Linux Vulkan support. Its Kokoro support
  must not be confused with the separate GPU-capable `simonfxr/kokoro.cpp` port.
  [TTS.cpp support matrix](https://github.com/mmwillet/TTS.cpp).
- Irodori TTS is a legitimate Vulkan-capable family with several sizes, but is
  Japanese-focused and does not meet Chatty's English-output requirement.
  [Native model catalog](https://github.com/nyosegawa/speech.cpp/blob/main/docs/models.md).
- The standard Fish Drama 3 35B Q4 package is about 20.8 GB before runtime memory,
  outside the current shared-memory headroom. Other heavily quantized or small
  experimental variants are separate investigations; CPU expert offloading does
  not meet the stated requirement. [Standard package](https://github.com/nyosegawa/speech.cpp/blob/main/docs/models.md),
  [experimental port and offloading recipes](https://github.com/0xShug0/audio.cpp/blob/main/docs/models/fish_drama3.md).
- KugelAudio's reliable F16 package is about 17.3 GB before scratch. Its native
  documentation rejects the smaller Q4 package as a quality substitute. It is
  not a practical candidate with the current free memory.
  [Runtime qualification](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md).
- Raon OpenTTS 0.3B/1B have native ports, but the documented CPU vocoder is a
  problem for the GPU requirement and latency. They need a separately verified
  GPU vocoder before joining the usable candidates.
  [Raon runtime notes](https://github.com/CrispStrobe/CrispASR/blob/main/docs/tts.md).
- A model with only a PyTorch CUDA example remains unqualified here unless it has
  an appropriate portable port or a demonstrated working ROCm setup. Native
  FlashAttention requirements and NVIDIA benchmark numbers must not be treated
  as AMD compatibility evidence.

AuK/other large instruction-guided generators and FireRedAudio's combined audio
tasks also appear in portable catalogs, but their exact English TTS paths,
memory, and GPU execution were not established. Do not list them as confirmed
hardware matches merely because they appear beside TTS models.

## Evaluation order and decision rule

1. **Soprano 80M, native Vulkan:** test genuine streamed first audio, intelligibility,
   and stability on short technical English. Its published Vega results make it
   a relevant architecture match, not a speed prediction for the 5825U.
2. **Kokoro 82M, native Vulkan:** compare English voices and short-phrase latency.
   Its mature fixed-voice workflow is attractive; full-utterance synthesis means
   phrase boundaries and queue continuity matter.
3. **Phonon 31M, explicit WebGPU:** compare if a small browser worker is acceptable.
   Keep it warm and require GPU mode to fail instead of silently using CPU.
4. **Supertonic 3, explicit WebGPU or qualified Vulkan port:** compare pronunciation
   and latency while accounting for its archived upstream.
5. **Qwen3-TTS 0.6B, native Vulkan:** evaluate if the lightweight engines are not
   natural or intelligible enough. Prefer a supported preset English voice to
   extra cloning machinery. Start with Q5/Q6/Q8 rather than assuming the smallest
   quantization preserves speech reliability.

This order is a reasoned shortlist from model size, runtime evidence, and the
conversation design. It is not a measured quality or speed ranking. The remaining
catalog preserves alternatives without treating generic backend labels as proof.

For each candidate, require:

- explicit selection of the real Radeon adapter, with no software or CPU fallback;
- logs or profiling showing the heavy neural synthesis stages and audio decoder
  on that adapter, with any host neural stages reported;
- cold and warm time to first **audible speech**, not merely the first zero-filled
  audio chunk;
- sustained generation faster than playback, plus peak shared-memory use;
- intelligible technical English, numbers, abbreviations, negations, and the
  acknowledgement “Sent it to Codex”;
- cancellation during playback and no stale audio after starting a new recording;
- repeated trials, including a warm Whisper backend resident and realistic
  desktop load, with short-phrase seams assessed by listening.

Begin with existing public preset voices. Keep the TTS adapter independent of the
chosen model so the measured winner can be selected without changing Chatty's
conversation or submission logic. The model choice remains pending these tests.
