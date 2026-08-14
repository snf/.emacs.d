#!/usr/bin/env python3
"""Relay connections from one Codex TUI and report its app-server thread id."""

from __future__ import annotations

import argparse
import asyncio
import contextlib
import json
from typing import Any

from websockets.asyncio.client import connect
from websockets.asyncio.server import ServerConnection, serve


def _emit(kind: str, **fields: Any) -> None:
    print(json.dumps({"type": kind, **fields}, separators=(",", ":")), flush=True)


def _parse_message(message: str | bytes) -> dict[str, Any] | None:
    try:
        value = json.loads(message)
    except (json.JSONDecodeError, UnicodeDecodeError, TypeError):
        return None
    return value if isinstance(value, dict) else None


def _thread_id_from_result(message: dict[str, Any]) -> str | None:
    result = message.get("result")
    if not isinstance(result, dict):
        return None
    thread = result.get("thread")
    if not isinstance(thread, dict):
        return None
    thread_id = thread.get("id")
    return thread_id if isinstance(thread_id, str) else None


async def _run_proxy(endpoint: str, host: str) -> None:
    reported_thread: str | None = None

    async def handler(client: ServerConnection) -> None:
        nonlocal reported_thread
        request_methods: dict[str, str] = {}

        def report(thread_id: str | None) -> None:
            nonlocal reported_thread
            if thread_id and thread_id != reported_thread:
                reported_thread = thread_id
                _emit("thread", thread_id=thread_id)

        try:
            async with connect(endpoint, compression=None, max_size=None) as upstream:

                async def toward_upstream() -> None:
                    async for raw in client:
                        message = _parse_message(raw)
                        if message is not None:
                            request_id = message.get("id")
                            method = message.get("method")
                            if isinstance(request_id, (str, int)) and method in {
                                "thread/start",
                                "thread/resume",
                            }:
                                request_methods[str(request_id)] = str(method)
                        await upstream.send(raw)

                async def toward_client() -> None:
                    async for raw in upstream:
                        message = _parse_message(raw)
                        if message is not None:
                            response_id = message.get("id")
                            if isinstance(
                                response_id, (str, int)
                            ) and request_methods.pop(str(response_id), None):
                                # A shared app-server can deliver lifecycle
                                # notifications for other threads.  Only a
                                # response to this TUI's own selection request
                                # identifies the buffer's thread.
                                report(_thread_id_from_result(message))
                        await client.send(raw)

                tasks = {
                    asyncio.create_task(toward_upstream()),
                    asyncio.create_task(toward_client()),
                }
                done, pending = await asyncio.wait(
                    tasks, return_when=asyncio.FIRST_COMPLETED
                )
                for task in pending:
                    task.cancel()
                await asyncio.gather(*done, *pending, return_exceptions=True)
        except Exception as exc:  # noqa: BLE001
            _emit("error", message=str(exc))
            with contextlib.suppress(Exception):
                await client.close(code=1011, reason="upstream connection failed")

    async with serve(handler, host, 0, compression=None, max_size=None) as server:
        socket = server.sockets[0]
        port = socket.getsockname()[1]
        _emit("ready", endpoint=f"ws://{host}:{port}")
        await asyncio.Future()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--endpoint", required=True)
    parser.add_argument("--host", default="127.0.0.1")
    args = parser.parse_args()
    try:
        asyncio.run(_run_proxy(args.endpoint, args.host))
    except KeyboardInterrupt:
        return 130
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
