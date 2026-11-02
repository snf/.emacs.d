#!/usr/bin/env python3
"""Restart the Emacs-managed shared stack after a delay and an idle window."""

from __future__ import annotations

import argparse
import asyncio
from datetime import datetime, timezone
import fcntl
import json
import os
from pathlib import Path
import shlex
import subprocess
import time
from urllib.request import urlopen

from websockets.asyncio.client import connect


EMACS_DIR = Path(__file__).resolve().parent.parent
RUNTIME_ROOT = EMACS_DIR / ".cache" / "codex"
STATUS_FILE = RUNTIME_ROOT / "restart-status.json"
LOG_FILE = RUNTIME_ROOT / "restart.log"
STATE_ROOT = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state"))
PID_FILE = STATE_ROOT / "takopi" / "mobile-stack.pid"


def now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def log(message: str) -> None:
    line = f"{now()} {message}"
    print(line, flush=True)
    with LOG_FILE.open("a", encoding="utf-8") as stream:
        stream.write(line + "\n")


def status(state: str, **details: object) -> None:
    temporary = STATUS_FILE.with_suffix(".json.tmp")
    temporary.write_text(
        json.dumps({"state": state, "updatedAt": now(), **details}, indent=2) + "\n",
        encoding="utf-8",
    )
    temporary.replace(STATUS_FILE)


def emacs_eval(expression: str) -> str:
    result = subprocess.run(
        ["emacsclient", "--eval", expression],
        check=True, capture_output=True, text=True, timeout=15,
    )
    return result.stdout.strip()


def settings() -> dict:
    expression = """(json-encode
      (list (cons "codex" codex-app-server-codex-program)
            (cons "endpoint" codex-app-server-endpoint)
            (cons "readyUrl" codex-app-server-ready-url)
            (cons "startCommand" (vconcat codex-app-server-start-command))))"""
    configured = json.loads(json.loads(emacs_eval(expression)))
    if configured["startCommand"][-1] != "start":
        raise RuntimeError("Expected the shared supervisor start command")
    if not os.access(configured["codex"], os.X_OK):
        raise RuntimeError(f"Configured Codex is not executable: {configured['codex']}")
    return configured


async def rpc(websocket, request_id: int, method: str, params: dict) -> dict:
    await websocket.send(json.dumps(dict(id=request_id, method=method, params=params)))
    async with asyncio.timeout(15):
        while True:
            message = json.loads(await websocket.recv())
            if message.get("id") == request_id:
                if "error" in message:
                    raise RuntimeError(f"{method}: {message['error']}")
                return message["result"]


async def initialize(websocket) -> None:
    await rpc(websocket, 1, "initialize", {
        "clientInfo": {"name": "emacs-codex-restart", "version": "1"},
        "capabilities": {"experimentalApi": True},
    })
    await websocket.send(json.dumps({"method": "initialized", "params": {}}))


async def active_threads(endpoint: str) -> list[str]:
    async with connect(endpoint, compression=None, max_size=None, open_timeout=5) as ws:
        await initialize(ws)
        request_id = 2
        thread_ids: list[str] = []
        params: dict = {}
        while True:
            loaded = await rpc(ws, request_id, "thread/loaded/list", params)
            request_id += 1
            thread_ids.extend(loaded["data"])
            if not loaded.get("nextCursor"):
                break
            params = {"cursor": loaded["nextCursor"]}
        active = []
        for thread_id in thread_ids:
            result = await rpc(ws, request_id, "thread/read", {
                "threadId": thread_id, "includeTurns": False,
            })
            request_id += 1
            thread_status = result["thread"]["status"]
            kind = thread_status.get("type") if isinstance(thread_status, dict) else thread_status
            if kind == "active":
                active.append(thread_id)
            elif kind not in {"idle", "notLoaded", "systemError"}:
                raise RuntimeError(f"Unknown thread status: {thread_status!r}")
        return active


def supervisor_pid() -> int | None:
    try:
        return int(PID_FILE.read_text().strip())
    except (OSError, ValueError):
        return None


def command(pid: int) -> list[str]:
    try:
        return Path(f"/proc/{pid}/cmdline").read_bytes().decode().rstrip("\0").split("\0")
    except OSError:
        return []


def children(pid: int) -> list[int]:
    try:
        return [int(p) for p in Path(f"/proc/{pid}/task/{pid}/children").read_text().split()]
    except OSError:
        return []


def descendants(pid: int) -> list[int]:
    found = []
    pending = children(pid)
    while pending:
        child = pending.pop()
        found.append(child)
        pending.extend(children(child))
    return found


def executable(pid: int) -> Path | None:
    try:
        return Path(f"/proc/{pid}/exe").resolve(strict=True)
    except OSError:
        return None


def configured_command(pid: int) -> list[str]:
    prefix = b"TAKOPI_STACK_CODEX_COMMAND="
    try:
        entries = Path(f"/proc/{pid}/environ").read_bytes().split(b"\0")
    except OSError:
        return []
    for entry in entries:
        if entry.startswith(prefix):
            return shlex.split(entry[len(prefix):].decode())
    return []


def native_binary(codex: str) -> Path:
    prefix = Path(codex).parent.parent.resolve()
    candidates = list(prefix.glob("lib/node_modules/**/codex-package.json"))
    if len(candidates) != 1:
        raise RuntimeError("Expected one complete official native runtime")
    metadata = json.loads(candidates[0].read_text())
    return (candidates[0].parent / metadata["entrypoint"]).resolve()


def wait_for_replacement(previous_pid: int | None, config: dict) -> dict:
    expected = [config["codex"], "app-server", "--listen", config["endpoint"]]
    expected_native = native_binary(config["codex"])
    expected_version = subprocess.check_output(
        [config["codex"], "--version"], text=True, timeout=10,
    ).strip()
    deadline = time.monotonic() + 120
    observation = "replacement has not started"
    while time.monotonic() < deadline:
        pid = supervisor_pid()
        if pid is None or pid == previous_pid or configured_command(pid) != expected:
            time.sleep(1)
            continue
        # Node versions without process.execve keep a launcher between the
        # supervisor and the native app-server. Validate the actual descendant.
        processes = descendants(pid)
        app_pid = next((p for p in processes if "app-server" in command(p)
                        and executable(p) == expected_native), None)
        bridge_pid = next((p for p in processes
                           if any(arg.endswith("/bin/takopi") for arg in command(p))), None)
        if app_pid is None or bridge_pid is None:
            observation = "app-server or Takopi child missing"
            time.sleep(1)
            continue
        try:
            with urlopen(config["readyUrl"], timeout=2) as response:
                if response.status != 200:
                    raise RuntimeError(f"readiness returned {response.status}")
            actual_version = subprocess.check_output(
                [f"/proc/{app_pid}/exe", "--version"], text=True, timeout=10,
            ).strip()
            if actual_version != expected_version:
                raise RuntimeError(f"version mismatch: {actual_version}")
            return dict(supervisorPid=pid, appServerPid=app_pid,
                        takopiPid=bridge_pid, version=actual_version,
                        executable=str(expected_native))
        except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
            observation = str(exc)
            time.sleep(1)
    raise RuntimeError(f"Replacement did not become healthy: {observation}")


async def tool_smoke_check(endpoint: str) -> None:
    async with connect(endpoint, compression=None, max_size=None, open_timeout=5) as ws:
        await initialize(ws)
        result = await rpc(ws, 2, "command/exec", {
            "command": ["/usr/bin/true"], "cwd": str(EMACS_DIR), "timeoutMs": 1000,
        })
        if result["exitCode"] != 0:
            raise RuntimeError(f"Harmless command failed: {result!r}")


async def main(delay_seconds: float, probe_only: bool) -> None:
    config = settings()
    if probe_only:
        print(json.dumps(dict(config=config, activeThreads=await active_threads(config["endpoint"])),
                         indent=2))
        return
    RUNTIME_ROOT.mkdir(parents=True, exist_ok=True)
    # Keep a second manually launched helper from racing the Emacs-owned one.
    with (RUNTIME_ROOT / "restart.lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        not_before = datetime.fromtimestamp(time.time() + delay_seconds, timezone.utc).isoformat()
        status("waiting", pid=os.getpid(), notBefore=not_before)
        log(f"restart requested; not before {not_before}")
        await asyncio.sleep(delay_seconds)
        idle_checks = 0
        last_active = None
        while idle_checks < 3:
            try:
                active = await active_threads(config["endpoint"])
                if active:
                    idle_checks = 0
                    if active != last_active:
                        log(f"waiting for {len(active)} active thread(s) to finish")
                    last_active = active
                    status("waiting-for-idle", pid=os.getpid(), activeThreads=active)
                    await asyncio.sleep(5)
                    continue
                idle_checks += 1
                status("waiting-for-idle", pid=os.getpid(), idleChecks=idle_checks)
                log(f"idle check {idle_checks}/3 passed")
                if idle_checks < 3:
                    await asyncio.sleep(5)
            except Exception as exc:
                idle_checks = 0
                log(f"idle probe failed; preserving running stack: {exc}")
                await asyncio.sleep(30)
        config = settings()  # Emacs remains the source of truth at activation.
        native_binary(config["codex"])  # Fail before stopping if the package is incomplete.
        previous_pid = supervisor_pid()
        status("restarting", previousSupervisorPid=previous_pid)
        subprocess.run([*config["startCommand"][:-1], "stop"],
                       check=True, capture_output=True, text=True, timeout=25)
        emacs_eval("(progn (codex-app-server-start-mobile-stack) t)")
        replacement = await asyncio.to_thread(wait_for_replacement, previous_pid, config)
        await tool_smoke_check(config["endpoint"])
        status("complete", **replacement, toolCheck="/usr/bin/true: exit 0")
        log(f"restart complete: {replacement}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--delay-seconds", type=float, default=0)
    parser.add_argument("--probe-only", action="store_true")
    args = parser.parse_args()
    if args.delay_seconds < 0:
        parser.error("delay must be nonnegative")
    try:
        asyncio.run(main(args.delay_seconds, args.probe_only))
    except Exception as exc:
        if not args.probe_only:
            status("failed", error=str(exc))
            log(f"FAILED: {exc}")
        raise
