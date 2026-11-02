# Emacs-managed official Codex migration

Date: 2026-11-02.

Emacs now uses `.cache/codex/npm/bin/codex`, an unversioned path to a private
official npm installation on the persistent Emacs mount. The current package
is `@openai/codex@0.166.0`; npm installed its complete same-version native
dependency and resources. The shell's global package/prefix was not changed.

- Upstream tag: `rust-v0.166.0`.
- Upstream commit: `6ff1e1ca3b30fc8b914720681fcf5fe62e95f856`.
- Source verification: the tagged session picker propagates the active remote
  server's CWD override through `with_remote_cwd_override`; no patch required.
- Codex SHA-256: `b8a11fedf24b2dfb59a70c9e52b030f9a508dcd49cd2dd0d41956f4c785156b8`.
- Code-mode host SHA-256: `657f4e902726e05867c937b583049d5997e057f8cd0ab40450633c9c9ef8d630`.

## Ownership and upgrades

`scripts/codex-install.sh` installs each official package in a fresh private npm
prefix and publishes it atomically via the `npm` symlink. Old package directories
are retained for processes that still need their companions. The Lisp setting,
Ghostel launcher, voice formatter, and Takopi command contain no release pin.
Emacs passes its exact executable to Takopi's existing supervisor; Takopi's
Python code required no change.

`scripts/codex-restart.py` and `codex-app-server-restart-mobile-stack` replace
the version-specific cached restart helper. They support immediate or delayed
activation, wait for three idle checks, explicitly stop/start through the
supervisor, and check actual native process identity, version, Takopi, readiness,
and `/usr/bin/true` through `command/exec`. The verifier handles Node launchers
and shell-quoted supervisor commands.

The earlier eight-hour task was cancelled and its status marked cancelled,
because the user requested migration followed by a restart now. A new restart
is armed with no time delay and is waiting for active turns to finish. Its
authoritative result is `.cache/codex/restart-status.json`, with details in
`.cache/codex/restart.log`; activation is not claimed complete before that result.

## Validation

- Full official CLI/native metadata checked; companion host `--help` passed.
- Installed native/host hashes match the previously verified official 0.166.0
  package.
- Ten focused Emacs lifecycle tests passed, including shared executable
  selection and scheduling without an immediate destructive restart.
- Three existing Takopi supervisor tests passed.
- Two focused helper tests passed: shell quoting and native processes below an
  older Node launcher.
- Live configuration/idle probe and harmless command against the old shared
  server passed without restarting it.
- A separate official managed app-server started through npm, initialized, ran
  `/usr/bin/true`, and exposed the expected native child. This used a temporary
  isolated `CODEX_HOME`; its default sandbox could not mount `/proc` in this
  container, so the isolated test used an explicit sandbox override for that
  known harmless command. The shared user configuration was not changed.
- Python syntax, shell syntax, and whitespace checks passed.
- No full test suites, Rust builds, or Clippy runs.

## Legacy cleanup

A `/proc` scan of executable paths, working directories, command arguments,
memory mappings, and open file paths confirmed that these exact installations
were unused before deletion:

- `.cache/codex/bin` (legacy flat patched binaries and host).
- `.cache/codex/releases/0.160.0`.
- `.cache/codex/releases/0.160.1`.
- `.cache/codex/releases/0.161.0`.
- `.cache/codex/releases/0.162.1`.
- `.cache/codex/releases/0.163.0`.
- `.cache/codex/releases/0.166.0` (superseded by the private npm installation).

Total allocated size removed: 7,164,555,264 bytes (about 6.67 GiB).

The complete 0.164.0 runtime remains because the old shared server and nine
existing TUIs used it during migration. Retain it until those TUIs close and
the new stack is verified; reopening a TUI selects the managed package.
Historical patches/build records and the dedicated Cargo cache were preserved.
