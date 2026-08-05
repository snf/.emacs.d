#!/usr/bin/env python3
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "codex-notify.py"


class CodexNotifyTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.state_dir = Path(self.tmp.name) / "attention"
        self.context_dir = Path(self.tmp.name) / "contexts"
        self.env = os.environ.copy()
        self.env.update(
            {
                "CODEX_ATTN_STATE_DIR": str(self.state_dir),
                "CODEX_CONTEXT_STATE_DIR": str(self.context_dir),
                "CODEX_ATTN_EMACS_INSTANCE_ID": "emacs-1",
                "CODEX_ATTN_TERMINAL_ID": "terminal-1",
            }
        )

    def tearDown(self):
        self.tmp.cleanup()

    def notify(self, event, env=None):
        subprocess.run(
            [sys.executable, str(SCRIPT), json.dumps(event)],
            check=True,
            env=env or self.env,
        )

    def read(self, thread="thread-1"):
        return json.loads((self.state_dir / f"{thread}.json").read_text())

    def read_context(self, terminal="terminal-1"):
        return json.loads((self.context_dir / f"{terminal}.json").read_text())

    def test_copies_required_process_identity(self):
        self.notify(
            {
                "type": "agent-turn-complete",
                "thread_id": "thread-1",
                "turn_id": "turn-1",
                "cwd": "/tmp/project",
            }
        )
        data = self.read()
        self.assertEqual(data["emacs_instance_id"], "emacs-1")
        self.assertEqual(data["terminal_id"], "terminal-1")

    def test_ignores_notifications_without_process_identity(self):
        env = self.env.copy()
        env.pop("CODEX_ATTN_EMACS_INSTANCE_ID")
        env.pop("CODEX_ATTN_TERMINAL_ID")
        self.notify(
            {
                "type": "agent-turn-complete",
                "thread_id": "thread-1",
                "turn_id": "turn-1",
            },
            env=env,
        )
        self.assertFalse(self.state_dir.exists())
        self.assertFalse(self.context_dir.exists())

    def test_persists_last_turn_context_by_terminal(self):
        self.notify(
            {
                "type": "agent-turn-complete",
                "thread-id": "thread-1",
                "turn-id": "turn-1",
                "cwd": "/tmp/project",
                "input-messages": ["Please compare both approaches"],
                "last-assistant-message": "Which approach should I implement?",
            }
        )
        data = self.read_context()
        self.assertEqual(data["thread_id"], "thread-1")
        self.assertEqual(data["input_messages"], ["Please compare both approaches"])
        self.assertEqual(
            data["last_assistant_message"], "Which approach should I implement?"
        )

    def test_exact_duplicate_does_not_rewrite(self):
        event = {
            "type": "agent-turn-complete",
            "thread_id": "thread-1",
            "turn_id": "turn-1",
        }
        self.notify(event)
        path = self.state_dir / "thread-1.json"
        before = path.stat().st_mtime_ns
        context_path = self.context_dir / "terminal-1.json"
        context_before = context_path.stat().st_mtime_ns
        time.sleep(0.02)
        self.notify(event)
        self.assertEqual(path.stat().st_mtime_ns, before)
        self.assertEqual(context_path.stat().st_mtime_ns, context_before)

    def test_new_turn_preserves_pending_since(self):
        base = {
            "type": "agent-turn-complete",
            "thread_id": "thread-1",
        }
        self.notify({**base, "turn_id": "turn-1"})
        before = self.read()
        time.sleep(0.02)
        self.notify({**base, "turn_id": "turn-2"})
        after = self.read()
        self.assertEqual(after["pending_since"], before["pending_since"])
        self.assertGreater(after["last_event_ts"], before["last_event_ts"])

    def test_concurrent_writers_leave_valid_json_and_no_temps(self):
        processes = []
        for number in range(12):
            event = {
                "type": "agent-turn-complete",
                "thread_id": "thread-1",
                "turn_id": f"turn-{number}",
            }
            processes.append(
                subprocess.Popen(
                    [sys.executable, str(SCRIPT), json.dumps(event)],
                    env=self.env,
                )
            )
        for process in processes:
            self.assertEqual(process.wait(), 0)
        self.assertEqual(self.read()["thread_id"], "thread-1")
        self.assertEqual(list(self.state_dir.glob("*.tmp")), [])
        self.assertEqual(self.read_context()["thread_id"], "thread-1")
        self.assertEqual(list(self.context_dir.glob("*.tmp")), [])


if __name__ == "__main__":
    unittest.main()
