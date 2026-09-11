import asyncio
import importlib.util
import json
from pathlib import Path
import sys
import unittest

from websockets.asyncio.client import connect
from websockets.asyncio.server import ServerConnection, serve


SCRIPT = Path(__file__).resolve().parents[1] / "codex-app-server-proxy.py"
spec = importlib.util.spec_from_file_location("codex_proxy", SCRIPT)
proxy = importlib.util.module_from_spec(spec)
spec.loader.exec_module(proxy)


class CodexAppServerProxyTests(unittest.IsolatedAsyncioTestCase):
    async def test_relays_overlapping_clients_and_reports_threads(self):
        async def upstream_handler(websocket: ServerConnection) -> None:
            async for raw in websocket:
                message = json.loads(raw)
                thread_id = message["params"]["threadId"]
                if message["method"] in {"thread/start", "thread/resume", "thread/fork"}:
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
                                "result": {"thread": {
                                    "id": thread_id,
                                    "ephemeral": message["params"].get("ephemeral", False),
                                }},
                            }
                        )
                    )
                elif message["method"] == "turn/start":
                    turn_id = f"turn-{message['id']}"
                    await websocket.send(
                        json.dumps(
                            {
                                "id": message["id"],
                                "result": {"turn": {"id": turn_id}},
                            }
                        )
                    )
                    await websocket.send(
                        json.dumps(
                            {
                                "method": "turn/completed",
                                "params": {"turn": {"id": turn_id, "status": "completed"}},
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

                    # The title generator uses another connection to this
                    # same proxy.  Its ephemeral thread must not steal the
                    # buffer binding or emit attention on completion.
                    async with connect(ready["endpoint"]) as title_client:
                        await title_client.send(json.dumps({
                            "id": "title-start", "method": "thread/start",
                            "params": {"threadId": "title-thread", "ephemeral": True},
                        }))
                        await title_client.recv()  # lifecycle notification
                        await title_client.recv()  # response
                        await title_client.send(json.dumps({
                            "id": "title-turn", "method": "turn/start",
                            "params": {"threadId": "title-thread"},
                        }))
                        await title_client.recv()
                        await title_client.recv()

                    await client.send(
                        json.dumps(
                            {
                            "id": f"turn-{request_id}",
                            "method": "turn/start",
                            "params": {"threadId": thread_id},
                            }
                        )
                    )
                    turn_response = json.loads(await client.recv())
                    self.assertEqual(turn_response["result"]["turn"]["id"], f"turn-turn-{request_id}")
                    completed = json.loads(await client.recv())
                    self.assertEqual(completed["method"], "turn/completed")
                    attention = json.loads(await asyncio.wait_for(process.stdout.readline(), 2))
                    self.assertEqual(
                        attention,
                        {
                            "type": "attention",
                            "thread_id": thread_id,
                            "turn_id": f"turn-turn-{request_id}",
                        },
                    )

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
                    await round_trip(active_tui, "request-3", "thread/fork", "forked-thread")

                self.assertIsNone(process.returncode)
            finally:
                process.terminate()
                await asyncio.wait_for(process.wait(), 2)

    async def test_live_binding_check_reads_only_and_rejects_ephemeral_threads(self):
        requests = []

        async def handler(websocket):
            async for raw in websocket:
                message = json.loads(raw)
                requests.append(message["method"])
                if message["method"] == "initialized":
                    continue
                result = {}
                if message["method"] == "thread/read":
                    self.assertFalse(message["params"]["includeTurns"])
                    thread_id = message["params"]["threadId"]
                    if thread_id == "unloaded-title":
                        await websocket.send(json.dumps({"id": message["id"], "error": {"code": -32600}}))
                        continue
                    result = {"thread": {"id": thread_id, "ephemeral": thread_id == "title"}}
                await websocket.send(json.dumps({"id": message["id"], "result": result}))

        async with serve(handler, "127.0.0.1", 0) as server:
            endpoint = f"ws://127.0.0.1:{server.sockets[0].getsockname()[1]}"
            self.assertTrue(await proxy._thread_is_durable(endpoint, "main"))
            self.assertFalse(await proxy._thread_is_durable(endpoint, "title"))
            self.assertFalse(await proxy._thread_is_durable(endpoint, "unloaded-title"))
        self.assertEqual(requests, ["initialize", "initialized", "thread/read"] * 3)


if __name__ == "__main__":
    unittest.main()
