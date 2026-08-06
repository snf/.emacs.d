#!/usr/bin/env python3
import json
import os
import sys
import tempfile
import time


CONTEXT_MESSAGE_MAX_CHARS = 16_384
CONTEXT_TRUNCATION_MARKER = "\n...[truncated]...\n"


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


def _read_json_file(path):
    try:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
        return data if isinstance(data, dict) else None
    except (OSError, ValueError, TypeError):
        return None


def _atomic_write_json(path, data):
    directory = os.path.dirname(path)
    os.makedirs(directory, mode=0o700, exist_ok=True)
    fd, tmp_path = tempfile.mkstemp(
        prefix=f".{os.path.basename(path)}.", suffix=".tmp", dir=directory
    )
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=True)
        os.replace(tmp_path, path)
    finally:
        try:
            os.unlink(tmp_path)
        except FileNotFoundError:
            pass


def _same_completed_turn(old, data):
    turn_id = data.get("turn_id")
    return (
        turn_id is not None
        and isinstance(old, dict)
        and old.get("thread_id") == data.get("thread_id")
        and old.get("provider") == data.get("provider")
        and old.get("turn_id") == turn_id
        and old.get("emacs_instance_id") == data.get("emacs_instance_id")
        and old.get("terminal_id") == data.get("terminal_id")
    )


def _bounded_context_text(value):
    if not isinstance(value, str):
        return None
    if len(value) <= CONTEXT_MESSAGE_MAX_CHARS:
        return value

    remaining = CONTEXT_MESSAGE_MAX_CHARS - len(CONTEXT_TRUNCATION_MARKER)
    head = remaining // 2
    tail = remaining - head
    return value[:head] + CONTEXT_TRUNCATION_MARKER + value[-tail:]


def _last_user_message(value):
    if isinstance(value, str):
        return _bounded_context_text(value)
    if not isinstance(value, list):
        return None

    for message in reversed(value):
        if isinstance(message, str):
            return _bounded_context_text(message)
        if isinstance(message, dict):
            for key in ("text", "content", "message"):
                text = message.get(key)
                if isinstance(text, str):
                    return _bounded_context_text(text)
    return None


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

    emacs_instance_id = os.environ.get("CODEX_ATTN_EMACS_INSTANCE_ID")
    terminal_id = os.environ.get("CODEX_ATTN_TERMINAL_ID")
    if not emacs_instance_id or not terminal_id:
        return 0

    state_dir = configured_state_dir
    if not state_dir:
        cache_home = os.environ.get("XDG_CACHE_HOME", os.path.expanduser("~/.cache"))
        state_dir = os.path.join(cache_home, provider, "threads")
    os.makedirs(state_dir, exist_ok=True)

    path = os.path.join(state_dir, f"{thread_id}.json")
    now = time.time()
    pending_since = now
    old = _read_json_file(path)
    if old:
        try:
            pending_since = float(old.get("pending_since", pending_since))
        except (TypeError, ValueError):
            pass

    turn_id = _pick_any(event, "turn_id", "turnId", "turn-id")
    input_messages = _pick_any(
        event,
        "input_messages",
        "inputMessages",
        "input-messages",
    )
    data = {
        "state_version": 2,
        "thread_id": thread_id,
        "provider": provider,
        "turn_id": turn_id,
        "cwd": _pick_any(
            event,
            "cwd",
            "working_directory",
            "working-directory",
            "workingDirectory",
            "path",
        ),
        "last_assistant_message": _bounded_context_text(
            _pick_any(
                event,
                "last_assistant_message",
                "lastAssistantMessage",
                "last-assistant-message",
                "assistant_message",
                "assistantMessage",
                "message",
            )
        ),
        "pending_since": pending_since,
        "last_event_ts": now,
        "type": _pick_any(event, "type", "event_type", "eventType"),
        "emacs_instance_id": emacs_instance_id,
        "terminal_id": terminal_id,
    }

    # Attention files are deliberately deleted once Emacs displays their
    # terminal.  Keep a separate per-terminal record so integrations such as
    # voice follow-ups can still ground the next message in the last completed
    # Codex turn.
    if provider == "codex":
        context_dir = os.environ.get("CODEX_CONTEXT_STATE_DIR")
        if not context_dir:
            cache_home = os.environ.get(
                "XDG_CACHE_HOME", os.path.expanduser("~/.cache")
            )
            context_dir = os.path.join(cache_home, "codex", "contexts")
        context_path = os.path.join(context_dir, f"{terminal_id}.json")
        context_old = _read_json_file(context_path)
        context_data = {
            **data,
            "context_version": 2,
            "last_user_message": _last_user_message(input_messages),
        }
        if (
            not isinstance(context_old, dict)
            or context_old.get("context_version") != 2
            or not _same_completed_turn(context_old, context_data)
        ):
            _atomic_write_json(context_path, context_data)

    # Codex can deliver the same completed-turn notification more than once.
    # Avoid rewriting the file in that case: besides needless I/O, every replace
    # wakes Emacs' file watcher.  A missing turn id is not safe to deduplicate.
    if (
        isinstance(old, dict)
        and old.get("state_version") == 2
        and _same_completed_turn(old, data)
    ):
        return 0

    _atomic_write_json(path, data)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
