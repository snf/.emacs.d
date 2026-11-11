"""Focused checks for package-launcher and shell-quoting restart pitfalls."""

import importlib.util
from pathlib import Path
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import MagicMock, patch

spec = importlib.util.spec_from_file_location(
    "codex_restart", Path(__file__).resolve().parents[1] / "scripts/codex-restart.py"
)
restart = importlib.util.module_from_spec(spec)
spec.loader.exec_module(restart)


class RestartChecks(unittest.TestCase):
    def test_children_include_processes_spawned_by_worker_threads(self):
        with TemporaryDirectory() as directory:
            proc = Path(directory)
            for thread, children in ((80, ""), (83, "151\n")):
                task = proc / "80" / "task" / str(thread)
                task.mkdir(parents=True)
                (task / "children").write_text(children)
            with patch.object(restart, "Path", side_effect=lambda path:
                              proc / Path(path).relative_to("/proc")):
                self.assertEqual(restart.children(80), [151])

    def test_supervisor_shell_quoting_does_not_change_the_command(self):
        environment = (
            b"OTHER=value\0TAKOPI_STACK_CODEX_COMMAND="
            b"/private\\ npm/bin/codex app-server --listen ws\\://127.0.0.1\\:4500\0"
        )
        with patch.object(Path, "read_bytes", return_value=environment):
            self.assertEqual(restart.configured_command(123), [
                "/private npm/bin/codex", "app-server", "--listen", "ws://127.0.0.1:4500",
            ])

    def test_health_check_finds_native_server_below_node_launcher(self):
        config = dict(codex="/managed/bin/codex", endpoint="ws://localhost:4500",
                      readyUrl="http://localhost:4500/readyz")
        native = Path("/package/bin/codex")
        process_children = {100: [101, 102], 101: [103], 102: [104]}
        process_commands = {
            101: ["node", "/managed/bin/codex", "app-server"],
            102: ["uv", "run", "takopi"],
            103: [str(native), "app-server"],
            104: ["python", "/venv/bin/takopi"],
        }
        response = MagicMock()
        response.__enter__.return_value.status = 200
        with (
            patch.object(restart, "supervisor_pid", return_value=100),
            patch.object(restart, "configured_command", return_value=[
                config["codex"], "app-server", "--listen", config["endpoint"],
            ]),
            patch.object(restart, "native_binary", return_value=native),
            patch.object(restart, "children", side_effect=lambda p: process_children.get(p, [])),
            patch.object(restart, "command", side_effect=lambda p: process_commands.get(p, [])),
            patch.object(restart, "executable", side_effect=lambda p: native if p == 103 else None),
            patch.object(restart.subprocess, "check_output", return_value="codex-cli test\n"),
            patch.object(restart, "urlopen", return_value=response),
        ):
            result = restart.wait_for_replacement(99, config)
        self.assertEqual(result["appServerPid"], 103)
        self.assertEqual(result["takopiPid"], 104)


if __name__ == "__main__":
    unittest.main()
