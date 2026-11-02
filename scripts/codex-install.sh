#!/usr/bin/env bash
# Install the official package privately; retain complete runtimes for live TUIs.
set -euo pipefail

emacs_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
runtime_root="$emacs_dir/.cache/codex"
mkdir -p "$runtime_root"
if [[ -e "$runtime_root/npm" && ! -L "$runtime_root/npm" ]]; then
  echo "Refusing to replace a non-symlink runtime: $runtime_root/npm" >&2
  exit 1
fi

version=$(npm view "@openai/codex@${1:-latest}" version)
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Expected one stable version, got: $version" >&2
  exit 1
fi
install_dir=$(mktemp -d "$runtime_root/.npm-install.XXXXXX")
echo "Installing official Codex $version into $install_dir"
npm install --global --prefix "$install_dir" --no-audit --no-fund \
  "@openai/codex@$version"

[[ "$("$install_dir/bin/codex" --version)" == "codex-cli $version" ]]
mapfile -t hosts < <(find "$install_dir/lib/node_modules" -type f \
  -name codex-code-mode-host)
mapfile -t metadata < <(find "$install_dir/lib/node_modules" -type f \
  -name codex-package.json)
if [[ ${#hosts[@]} -ne 1 || ${#metadata[@]} -ne 1 ]]; then
  echo "Expected one complete native runtime; installation retained for inspection" >&2
  exit 1
fi
jq -e --arg version "$version" \
  '.version == $version and .target == "x86_64-unknown-linux-musl"' \
  "${metadata[0]}" >/dev/null
"${hosts[0]}" --help >/dev/null
sha256sum "$(dirname -- "${hosts[0]}")/codex" "${hosts[0]}"

if [[ -L "$runtime_root/npm" ]]; then
  ln -s -- "$(readlink -- "$runtime_root/npm")" "$install_dir/previous-link"
  mv -Tf -- "$install_dir/previous-link" "$runtime_root/npm-previous"
fi
ln -s -- "$(basename -- "$install_dir")" "$install_dir/current-link"
mv -Tf -- "$install_dir/current-link" "$runtime_root/npm"
echo "Installed $version; Emacs uses $runtime_root/npm/bin/codex"
echo "Activate with M-x codex-app-server-restart-mobile-stack after approval."
