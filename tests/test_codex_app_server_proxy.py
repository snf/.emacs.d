import asyncio
import json
from pathlib import Path
import sys
import unittest

from websockets.asyncio.client import connect
from websockets.asyncio.server import ServerConnection, serve


SCRIPT = Path(__file__).resolve().parents[1] / "codex-app-server-proxy.py"


class CodexAppServerProxyTests(unittest.IsolatedAsyncioTestCase):
    async def test_relays_overlapping_clients_and_reports_threads(self):
        async def upstream_handler(websocket: ServerConnection) -> None:
            async for raw in websocket:
                message = json.loads(raw)
                thread_id = message["params"]["threadId"]
                await websocket.send(
                    json.dumps(
                        {
                            "method": "thread/started",
                            "params": {"thread": {"id": "unrelated-thread"}},
                        }
                    )
                )
                await websocket.send(
                    json.dumps(
                        {
                            "id": message["id"],
                            "result": {"thread": {"id": thread_id}},
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

            try:

                async def round_trip(client, request_id, method, thread_id):
                    request = {
                        "id": request_id,
                        "method": method,
                        "params": {"threadId": thread_id},
                    }
                    await client.send(json.dumps(request))
                    notification = json.loads(await client.recv())
                    self.assertEqual(notification["method"], "thread/started")
                    response = json.loads(await client.recv())
                    self.assertEqual(response["result"]["thread"]["id"], thread_id)
                    event = json.loads(await process.stdout.readline())
                    self.assertEqual(event, {"type": "thread", "thread_id": thread_id})

                async with connect(ready["endpoint"]) as active_tui:
                    await round_trip(
                        active_tui,
                        "request-1",
                        "thread/start",
                        "started-thread",
                    )
                    async with connect(ready["endpoint"]) as session_picker:
                        await round_trip(
                            session_picker,
                            "request-2",
                            "thread/resume",
                            "resumed-thread",
                        )

                self.assertIsNone(process.returncode)
            finally:
                process.terminate()
                await asyncio.wait_for(process.wait(), 2)


if __name__ == "__main__":
    unittest.main()
