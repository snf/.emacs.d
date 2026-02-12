#!/usr/bin/env python3
import json
import os
import sys
import time


def _pick(event, *keys):
    for key in keys:
        value = event.get(key)
        if value is not None:
            return value
    return None


def _pick_any(event, *keys):
    value = _pick(event, *keys)
    if value is not None:
        return value
    for parent_key in ("session", "data", "payload"):
        parent = event.get(parent_key)
        if isinstance(parent, dict):
            nested = _pick(parent, *keys)
            if nested is not None:
                return nested
    return None


def _normalize_provider(provider):
    text = (provider or "codex").strip().lower()
    if not text:
        return "codex"
    return text.replace(" ", "_")


def _parse_args():
    provider_env = os.environ.get("CODEX_ATTN_PROVIDER")
    provider = provider_env or "codex"
    provider_explicit = bool(provider_env)
    state_dir = os.environ.get("CODEX_ATTN_STATE_DIR")
    payload_arg = None

    args = iter(sys.argv[1:])
    for arg in args:
        if arg in ("-p", "--provider"):
            provider = next(args, provider)
            provider_explicit = True
        elif arg.startswith("--provider="):
            provider = arg.split("=", 1)[1]
            provider_explicit = True
        elif arg == "--state-dir":
            state_dir = next(args, state_dir)
        elif arg.startswith("--state-dir="):
            state_dir = arg.split("=", 1)[1]
        elif payload_arg is None:
            payload_arg = arg

    return _normalize_provider(provider), state_dir, payload_arg, provider_explicit


def _read_payload(payload_arg):
    if payload_arg:
        return payload_arg

    # Newer hook runners may send payload on stdin instead of argv.
    stdin_text = sys.stdin.read()
    if stdin_text:
        return stdin_text.strip()

    return ""


def _is_turn_complete(event):
    event_type = _pick_any(event, "type", "event_type", "eventType")
    if not event_type:
        # Some payload variants are already scoped to turn completion.
        return _pick_any(
            event,
            "thread_id",
            "threadId",
            "thread-id",
            "session_id",
            "sessionId",
            "sessionID",
            "id",
        ) is not None

    normalized = (
        str(event_type)
        .strip()
        .lower()
        .replace("-", "_")
        .replace(".", "_")
        .replace(" ", "_")
    )

    if normalized in (
        "agent-turn-complete",
        "agent_turn_complete",
        "turn-complete",
        "turn_complete",
        "session-completed",
        "session_completed",
        "session-complete",
        "session_complete",
        "completed",
    ):
        return True

    return normalized.endswith("_completed") or normalized.endswith("_complete")


def _debug_log(provider, payload, event):
    debug_path = os.environ.get("CODEX_ATTN_DEBUG_FILE")
    if not debug_path:
        return
    try:
        with open(debug_path, "a", encoding="utf-8") as f:
            f.write(
                json.dumps(
                    {
                        "ts": time.time(),
                        "provider": provider,
                        "argv": sys.argv,
                        "payload": payload,
                        "event": event,
                    },
                    ensure_ascii=True,
                )
            )
            f.write("\n")
    except Exception:
        pass


def main():
    provider, configured_state_dir, payload_arg, provider_explicit = _parse_args()
    payload = _read_payload(payload_arg)
    if not payload:
        return 0

    try:
        event = json.loads(payload)
    except json.JSONDecodeError:
        return 0

    if not isinstance(event, dict):
        return 0

    if not provider_explicit:
        hook_source = _pick_any(event, "hook_source", "hookSource")
        event_provider = _pick_any(event, "provider", "source")
        if isinstance(hook_source, str) and hook_source.strip().lower() == "opencode-plugin":
            provider = "opencode"
        elif isinstance(event_provider, str):
            provider = _normalize_provider(event_provider)

    _debug_log(provider, payload, event)

    if not _is_turn_complete(event):
        return 0

    thread_id = _pick_any(
        event,
        "thread_id",
        "threadId",
        "thread-id",
        "session_id",
        "sessionId",
        "sessionID",
        "id",
    )
    if not thread_id:
        return 0

    state_dir = configured_state_dir
    if not state_dir:
        cache_home = os.environ.get("XDG_CACHE_HOME", os.path.expanduser("~/.cache"))
        state_dir = os.path.join(cache_home, provider, "threads")
    os.makedirs(state_dir, exist_ok=True)

    path = os.path.join(state_dir, f"{thread_id}.json")
    now = time.time()
    pending_since = now
    if os.path.exists(path):
        try:
            with open(path, "r", encoding="utf-8") as f:
                old = json.load(f)
            pending_since = float(old.get("pending_since", pending_since))
        except Exception:
            pass

    data = {
        "thread_id": thread_id,
        "provider": provider,
        "turn_id": _pick_any(event, "turn_id", "turnId", "turn-id"),
        "cwd": _pick_any(
            event,
            "cwd",
            "working_directory",
            "working-directory",
            "workingDirectory",
            "path",
        ),
        "last_assistant_message": _pick_any(
            event,
            "last_assistant_message",
            "lastAssistantMessage",
            "last-assistant-message",
            "assistant_message",
            "assistantMessage",
            "message",
        ),
        "pending_since": pending_since,
        "last_event_ts": now,
        "type": _pick_any(event, "type", "event_type", "eventType"),
    }

    tmp_path = path + ".tmp"
    with open(tmp_path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=True)
    os.replace(tmp_path, path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
