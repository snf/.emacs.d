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


def _read_payload():
    if len(sys.argv) >= 2 and sys.argv[1]:
        return sys.argv[1]

    # Newer hook runners may send payload on stdin instead of argv.
    stdin_text = sys.stdin.read()
    if stdin_text:
        return stdin_text.strip()

    return ""


def _is_turn_complete(event):
    event_type = _pick(event, "type", "event_type", "eventType")
    if not event_type:
        # Some payload variants are already scoped to turn completion.
        return _pick(event, "thread_id", "threadId", "thread-id") is not None

    return event_type in (
        "agent-turn-complete",
        "agent_turn_complete",
        "turn-complete",
        "turn_complete",
    )


def main():
    payload = _read_payload()
    if not payload:
        return 0

    try:
        event = json.loads(payload)
    except json.JSONDecodeError:
        return 0

    if not isinstance(event, dict):
        return 0

    if not _is_turn_complete(event):
        return 0

    thread_id = _pick(event, "thread_id", "threadId", "thread-id")
    if not thread_id:
        return 0

    cache_home = os.environ.get("XDG_CACHE_HOME", os.path.expanduser("~/.cache"))
    state_dir = os.path.join(cache_home, "codex", "threads")
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
        "turn_id": _pick(event, "turn_id", "turnId", "turn-id"),
        "cwd": _pick(event, "cwd", "working_directory", "working-directory"),
        "last_assistant_message": _pick(
            event,
            "last_assistant_message",
            "lastAssistantMessage",
            "last-assistant-message",
        ),
        "pending_since": pending_since,
        "last_event_ts": now,
        "type": _pick(event, "type", "event_type", "eventType"),
    }

    tmp_path = path + ".tmp"
    with open(tmp_path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=True)
    os.replace(tmp_path, path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
