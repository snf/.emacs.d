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
    # The TUI opens an ephemeral thread to generate a conversation title.
    # It is an auxiliary request, not a change of the displayed conversation.
    if thread.get("ephemeral") is True:
        return None
    thread_id = thread.get("id")
    return thread_id if isinstance(thread_id, str) else None


def _turn_id_from_result(message: dict[str, Any]) -> str | None:
    result = message.get("result")
    if not isinstance(result, dict):
        return None
    turn = result.get("turn")
    if not isinstance(turn, dict):
        return None
    turn_id = turn.get("id")
    return turn_id if isinstance(turn_id, str) else None


def _turn_id_from_notification(message: dict[str, Any]) -> str | None:
    params = message.get("params")
    if not isinstance(params, dict):
        return None
    turn = params.get("turn")
    if isinstance(turn, dict) and isinstance(turn.get("id"), str):
        return turn["id"]
    turn_id = params.get("turnId")
    return turn_id if isinstance(turn_id, str) else None


def _thread_id_from_notification(message: dict[str, Any]) -> str | None:
    params = message.get("params")
    if not isinstance(params, dict):
        return None
    thread = params.get("thread")
    if isinstance(thread, dict) and isinstance(thread.get("id"), str):
        return thread["id"]
    thread_id = params.get("threadId")
    return thread_id if isinstance(thread_id, str) else None


async def _run_proxy(endpoint: str, host: str) -> None:
    reported_thread: str | None = None

    async def handler(client: ServerConnection) -> None:
        nonlocal reported_thread
        request_context: dict[str, tuple[str, str | None]] = {}
        turn_threads: dict[str, str] = {}

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
                                "thread/fork",
                                "turn/start",
                            }:
                                params = message.get("params")
                                thread_id = (
                                    params.get("threadId")
                                    if isinstance(params, dict)
                                    else None
                                )
                                request_context[str(request_id)] = (
                                    str(method),
                                    thread_id if isinstance(thread_id, str) else None,
                                )
                        await upstream.send(raw)

                async def toward_client() -> None:
                    async for raw in upstream:
                        message = _parse_message(raw)
                        if message is not None:
                            response_id = message.get("id")
                            context = (
                                request_context.pop(str(response_id), None)
                                if isinstance(response_id, (str, int))
                                else None
                            )
                            if context is not None:
                                method, request_thread_id = context
                                if method in {"thread/start", "thread/resume", "thread/fork"}:
                                    # A shared app-server can deliver lifecycle
                                    # notifications for other threads.  Only a
                                    # response to this TUI's own selection request
                                    # identifies the buffer's thread.
                                    report(_thread_id_from_result(message))
                                elif method == "turn/start":
                                    turn_id = _turn_id_from_result(message)
                                    thread_id = request_thread_id or reported_thread
                                    if turn_id and thread_id:
                                        turn_threads[turn_id] = thread_id
                            if message.get("method") == "turn/completed":
                                turn_id = _turn_id_from_notification(message)
                                thread_id = (
                                    turn_threads.pop(turn_id, None)
                                    if turn_id
                                    else None
                                )
                                thread_id = thread_id or _thread_id_from_notification(message)
                                # Report only the displayed durable thread.
                                # This excludes title-generation turns and
                                # unrelated shared-server notifications.
                                if thread_id and thread_id == reported_thread:
                                    _emit(
                                        "attention",
                                        thread_id=thread_id,
                                        turn_id=turn_id,
                                    )
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
        _emit("ready", endpoint=f"ws://{host}:{port}", filters_ephemeral=True)
        await asyncio.Future()


async def _thread_is_durable(endpoint: str, thread_id: str) -> bool:
    """Validate bindings from older proxies without restarting their sockets."""
    async with connect(endpoint, compression=None, max_size=None) as websocket:
        async def request(request_id: int, method: str, params: dict) -> dict:
            await websocket.send(json.dumps(dict(id=request_id, method=method, params=params)))
            async for raw in websocket:
                message = _parse_message(raw)
                if message and message.get("id") == request_id:
                    return message
            return {}

        initialized = await request(1, "initialize", {
            "clientInfo": {"name": "codex-attn", "version": "1"},
            "capabilities": {"experimentalApi": True},
        })
        if "result" not in initialized:
            return False
        await websocket.send(json.dumps({"method": "initialized", "params": {}}))
        response = await request(2, "thread/read", {
            "threadId": thread_id, "includeTurns": False,
        })
        return _thread_id_from_result(response) == thread_id


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--endpoint", required=True)
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--check-thread")
    args = parser.parse_args()
    try:
        if args.check_thread:
            durable = asyncio.run(asyncio.wait_for(
                _thread_is_durable(args.endpoint, args.check_thread), 10
            ))
            return 0 if durable else 1
        asyncio.run(_run_proxy(args.endpoint, args.host))
    except KeyboardInterrupt:
        return 130
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
