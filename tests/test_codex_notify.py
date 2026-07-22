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
        self.state_dir = Path(self.tmp.name)
        self.env = os.environ.copy()
        self.env.update(
            {
                "CODEX_ATTN_STATE_DIR": str(self.state_dir),
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
        self.assertEqual(list(self.state_dir.iterdir()), [])

    def test_exact_duplicate_does_not_rewrite(self):
        event = {
            "type": "agent-turn-complete",
            "thread_id": "thread-1",
            "turn_id": "turn-1",
        }
        self.notify(event)
        path = self.state_dir / "thread-1.json"
        before = path.stat().st_mtime_ns
        time.sleep(0.02)
        self.notify(event)
        self.assertEqual(path.stat().st_mtime_ns, before)

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


if __name__ == "__main__":
    unittest.main()
