import asyncio
import json
from pathlib import Path
import sys
import unittest

from websockets.asyncio.client import connect
from websockets.asyncio.server import ServerConnection, serve


SCRIPT = Path(__file__).resolve().parents[1] / "codex-app-server-proxy.py"


class CodexAppServerProxyTests(unittest.IsolatedAsyncioTestCase):
    async def test_relays_protocol_and_reports_started_thread(self):
        async def upstream_handler(websocket: ServerConnection) -> None:
            async for raw in websocket:
                message = json.loads(raw)
                await websocket.send(
                    json.dumps(
                        {
                            "id": message["id"],
                            "result": {"thread": {"id": "thread-from-server"}},
                        }
                    )
                )

        async with serve(upstream_handler, "127.0.0.1", 0) as upstream:
            upstream_port = upstream.sockets[0].getsockname()[1]
            process = await asyncio.create_subprocess_exec(
                sys.executable,
                str(SCRIPT),
                "--endpoint",
                f"ws://127.0.0.1:{upstream_port}",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            assert process.stdout is not None
            ready = json.loads(await process.stdout.readline())
            self.assertEqual(ready["type"], "ready")

            async with connect(ready["endpoint"]) as client:
                request = {
                    "id": "request-1",
                    "method": "thread/start",
                    "params": {"cwd": "/tmp/project"},
                }
                await client.send(json.dumps(request))
                response = json.loads(await client.recv())
                self.assertEqual(
                    response["result"]["thread"]["id"], "thread-from-server"
                )
                event = json.loads(await process.stdout.readline())
                self.assertEqual(
                    event, {"type": "thread", "thread_id": "thread-from-server"}
                )

            self.assertEqual(await asyncio.wait_for(process.wait(), 2), 0)


if __name__ == "__main__":
    unittest.main()
