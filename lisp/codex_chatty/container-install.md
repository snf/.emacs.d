# Kokoro installation in the development container

Add this block after the supplied Whisper installation, while the Docker build
runs as root. It reuses Whisper's Vulkan headers, Mesa drivers, shader compiler,
and CMake. The installed runtime and model files live under `/opt/kokoro.cpp`.
The actual Dockerfile is outside this workspace and has not been edited.

```dockerfile
# Kokoro dependencies in addition to the existing Whisper/Vulkan packages.
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential git curl ca-certificates pkg-config \
      libespeak-ng-dev espeak-ng espeak-ng-data pulseaudio-utils \
    && rm -rf /var/lib/apt/lists/*

# The tested runtime; checkout before initializing its pinned submodules.
ARG KOKORO_REV=a9e31430838c8bdc3d4beaaab62753681b9f7839
RUN git clone --no-checkout https://github.com/simonfxr/kokoro.cpp.git /opt/kokoro.cpp \
    && git -C /opt/kokoro.cpp checkout --detach "${KOKORO_REV}" \
    && git -C /opt/kokoro.cpp submodule update --init --recursive --depth 1 \
    && cmake -S /opt/kokoro.cpp -B /opt/kokoro.cpp/build \
         -DCMAKE_BUILD_TYPE=Release \
         -DKOKORO_VULKAN=ON \
         -DKOKORO_CUDA=OFF \
         -DKOKORO_METAL=OFF \
         -DKOKORO_STATIC=OFF \
         -DKOKORO_BUILD_CLI=ON \
         -DKOKORO_BUILD_TESTS=OFF \
    && cmake --build /opt/kokoro.cpp/build --config Release -j"$(nproc)" \
    && test -r /opt/kokoro.cpp/build/libkokoro.so \
    && ln -s "/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH)/espeak-ng-data" \
         /opt/kokoro.cpp/espeak-ng-data \
    && test -r /opt/kokoro.cpp/espeak-ng-data/phondata

# Pin and verify the exact F16 model and English voice used in our comparisons.
ARG KOKORO_MODELS_REV=c64650d0950e0b718b6eeb2f18620e25daf5c90b
RUN curl --fail --location --retry 3 \
      "https://huggingface.co/simonfxr/kokoro.cpp-GGUF/resolve/${KOKORO_MODELS_REV}/kokoro-82m-f16.gguf" \
      --output /opt/kokoro.cpp/models/kokoro-82m-f16.gguf \
    && curl --fail --location --retry 3 \
      "https://huggingface.co/simonfxr/kokoro.cpp-GGUF/resolve/${KOKORO_MODELS_REV}/voices/kokoro-voice-af_heart.gguf" \
      --output /opt/kokoro.cpp/models/kokoro-voice-af_heart.gguf \
    && cd /opt/kokoro.cpp/models \
    && printf '%s\n' \
      '597926de84f5550e1526ce0abde4e496209d464afc9274d8603d14f3c04d1f67  kokoro-82m-f16.gguf' \
      'c2f44076dfb8f9c098a85d634f6d6b46b038f80e3afdf695ff3b90f6d9ef473f  kokoro-voice-af_heart.gguf' \
      | sha256sum --check -

# espeak_Initialize expects the parent of the espeak-ng-data directory.
ENV KOKORO_ESPEAK_DATA_PATH=/opt/kokoro.cpp
```

`KOKORO_STATIC=OFF` produces the `libkokoro.so` loaded by the Python worker.
`KOKORO_VULKAN=ON` enables Vulkan in the bundled GGML build. Both the GGML and
Highway revisions are pinned by the selected Kokoro commit. The voice download
uses the repository's `voices/` directory. No Python model conversion or
PyTorch installation is needed for these preconverted GGUF files.

`pulseaudio-utils` supplies `pacat` for capture/playback through the existing
host PulseAudio connection. Retain the current container's audio socket and GPU
device access. Model loading and synthesis happen at container runtime; the
image build does not require access to the physical GPU.

After rebuilding the container, configure the existing Chatty Emacs integration
to use the installed files (for example in the `codex-chatty` use-package stanza):

```elisp
:custom
(codex-chatty-tts-options
 '((library . "/opt/kokoro.cpp/build/libkokoro.so")
   (model . "/opt/kokoro.cpp/models/kokoro-82m-f16.gguf")
   (voice . "/opt/kokoro.cpp/models/kokoro-voice-af_heart.gguf")))
```

Apply those settings when deploying the image containing these paths. The
current Emacs session continues using the already tested local assets until
then. All installed files only need to be readable by the Emacs user.

The worker already selects Vulkan device 0, sets
`GGML_VK_ALLOW_GRAPHICS_QUEUE=1` before GPU initialization, keeps Kokoro warm,
and uses the approved native `length_scale=1/1.526`. These settings belong to
the Kokoro worker; no global Vulkan queue environment change is needed.

The existing Python command also needs `websockets`, already present in the
Takopi environment. This block installs the native speech runtime and audio
utilities; it does not change that Python environment.

Validation in this session: inspected the pinned CMake options and native API;
matched both local model SHA-256 values to the publisher's pinned Hugging Face
metadata; checked the shell syntax and Emacs settings; exercised the actual GPU
worker with explicitly configured library/model/voice paths. A complete Docker
image build has not been run: Docker and Podman are unavailable here, and the
actual Dockerfile is outside the workspace.

Sources: [pinned runtime build configuration](https://github.com/simonfxr/kokoro.cpp/blob/a9e31430838c8bdc3d4beaaab62753681b9f7839/CMakeLists.txt),
[pinned model publisher metadata](https://huggingface.co/api/models/simonfxr/kokoro.cpp-GGUF/revision/c64650d0950e0b718b6eeb2f18620e25daf5c90b?blobs=true).
