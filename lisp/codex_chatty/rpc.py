"""Multiplexed app-server client; replies, events and tool calls never block each other."""
import asyncio
import json
import uuid

from websockets.asyncio.client import connect


class RPCError(RuntimeError):
    pass


class RPC:
    def __init__(self, endpoint, event, server_request):
        self.endpoint, self.event, self.server_request = endpoint, event, server_request
        self.pending = {}
        self.calls = set()
        self.ws = self.reader = None
        self.prefix = uuid.uuid4().hex[:10]
        self.serial = 0
        self.closed = False

    async def connect(self):
        self.ws = await connect(self.endpoint, compression=None, max_size=32*1024*1024,
                                open_timeout=10, ping_interval=20)
        self.reader = asyncio.create_task(self._read())
        await self.request('initialize', dict(
            clientInfo=dict(name='emacs_chatty', title='Emacs Chatty Codex', version='0.1.0'),
            capabilities=dict(experimentalApi=True)))
        await self.send(dict(method='initialized'))

    async def send(self, value):
        await self.ws.send(json.dumps(value))

    async def request(self, method, params, timeout=45):
        if self.closed:
            raise RPCError('Codex connection is closed')
        self.serial += 1
        key = f'{self.prefix}-{self.serial}'
        future = asyncio.get_running_loop().create_future()
        self.pending[key] = future
        try:
            await self.send(dict(id=key, method=method, params=params))
            return await asyncio.wait_for(asyncio.shield(future), timeout)
        finally:
            self.pending.pop(key, None)
            if not future.done(): future.cancel()

    async def _server_call(self, message):
        try:
            result = await self.server_request(message)
            await self.send(dict(id=message['id'], result=result))
        except Exception as error:
            if not self.closed:
                try:
                    await self.send(dict(id=message['id'], error=dict(code=-32603, message=str(error))))
                except Exception:
                    pass

    async def _read(self):
        error = RPCError('Codex disconnected; do not automatically resend a handoff')
        try:
            async for raw in self.ws:
                message = json.loads(raw)
                if 'method' in message:
                    if 'id' in message:
                        task = asyncio.create_task(self._server_call(message))
                        self.calls.add(task)
                        task.add_done_callback(self.calls.discard)
                    else:
                        self.event(message)
                else:
                    future = self.pending.get(message.get('id'))
                    if future and not future.done():
                        if 'error' in message:
                            future.set_exception(RPCError(str(message['error'])))
                        else:
                            future.set_result(message.get('result', {}))
        except asyncio.CancelledError:
            return
        except Exception as exception:
            error = RPCError(str(exception))
        finally:
            self.closed = True
            for future in list(self.pending.values()):
                if not future.done(): future.set_exception(error)
            self.event(dict(method='chatty/connectionLost', params=dict(message=str(error))))

    async def close(self):
        self.closed = True
        if self.ws: await self.ws.close()
        if self.reader:
            await asyncio.gather(self.reader, return_exceptions=True)
        for task in list(self.calls): task.cancel()
        await asyncio.gather(*self.calls, return_exceptions=True)


def discussion_config(server_config):
    """Per-thread overrides. Never change the shared server's global configuration."""
    overrides = {
        'project_doc_max_bytes': 0,
        'model_reasoning_effort': 'medium',
        'web_search': 'live',
        'skills.include_instructions': False,
        'include_apps_instructions': False,
    }
    for flag in ['shell_tool', 'apps', 'plugins', 'remote_plugin', 'multi_agent',
                 'multi_agent_v2', 'multi_agent_mode', 'multi_agent_v2_dynamic_tools',
                 'goals', 'memory_tool', 'hooks', 'codex_hooks', 'plugin_hooks',
                 'code_mode', 'code_mode_only', 'code_mode_host', 'browser_use']:
        overrides['features.'+flag] = False
    overrides['features.skip_host_skill_discovery'] = True
    # Disable each inherited server explicitly; an empty map can merge with defaults.
    for name in server_config.get('mcp_servers', {}):
        overrides[f'mcp_servers.{name}.enabled'] = False
    return overrides
