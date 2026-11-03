"""Narrow project reads and fixed schemas; no command execution tool."""
import json
import os
from pathlib import Path
import selectors
import subprocess
import time


def function(name, description, properties, required):
    return dict(type='function', name=name, description=description,
                inputSchema=dict(type='object', properties=properties,
                                 required=required, additionalProperties=False))


TOOLS = [
    function('read_project_file', 'Read a text file in the bound project. Does not edit or submit anything.',
             dict(path=dict(type='string'), start_line=dict(type='integer', minimum=1),
                  max_lines=dict(type='integer', minimum=1, maximum=300)), ['path']),
    function('search_project', 'Search text or file paths inside the bound project, read-only.',
             dict(query=dict(type='string'), files_only=dict(type='boolean')), ['query']),
    function('submit_to_working_codex',
             'Only after explicit user delegation, send the complete English instruction to the pinned working Codex. '
             'Default steer joins an active task; after_current waits until it finishes. The application speaks acknowledgement.',
             dict(instruction=dict(type='string'), delivery=dict(type='string', enum=['steer', 'after_current'])), ['instruction']),
    function('end_chatty_session', 'Close this discussion only when the user explicitly ends it; submit nothing.', {}, []),
]


class ProjectReads:
    def __init__(self, cwd):
        self.root = Path(cwd).resolve()

    def read(self, arguments):
        path = (self.root / arguments['path']).resolve()
        if not path.is_relative_to(self.root):
            raise ValueError('Read path is outside the bound project')
        if not path.is_file(): raise ValueError('Not a readable project file')
        start = max(1, int(arguments.get('start_line', 1)))
        maximum = min(300, max(1, int(arguments.get('max_lines', 150))))
        if path.stat().st_size > 5*1024*1024:
            raise ValueError('File is too large for a conversational read; use search_project')
        lines = path.read_text().splitlines()
        selected = lines[start-1:start-1+maximum]
        output = '\n'.join(f'{start+i}: {line}' for i, line in enumerate(selected))
        return dict(path=str(path.relative_to(self.root)), content=output[:24000], total_lines=len(lines))

    def search(self, arguments):
        query = arguments['query']
        if not isinstance(query, str) or not query.strip(): raise ValueError('Search query is empty')
        if arguments.get('files_only'):
            command = ['rg', '--files', '--glob', query, '.']
        else:
            command = ['rg', '-n', '--max-count', '12', '--max-columns', '300', '--', query, '.']
        # Stop reading and terminate rg once the conversational result is full.
        # Both pipes are drained to avoid a noisy stderr blocking the search.
        output, errors = bytearray(), bytearray()
        truncated = False
        with subprocess.Popen(command, cwd=self.root, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE) as process, selectors.DefaultSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ, output)
            selector.register(process.stderr, selectors.EVENT_READ, errors)
            deadline = time.monotonic()+10
            try:
                while selector.get_map():
                    remaining = deadline-time.monotonic()
                    if remaining <= 0:
                        raise TimeoutError('Project search exceeded ten seconds; narrow the query')
                    for key, _ in selector.select(remaining):
                        data = os.read(key.fileobj.fileno(), 8192)
                        if not data:
                            selector.unregister(key.fileobj)
                            continue
                        limit = 24001 if key.data is output else 1000
                        key.data.extend(data[:max(0, limit-len(key.data))])
                    if len(output)>24000:
                        truncated = True
                        break
                if not truncated:
                    process.wait(timeout=max(.01, deadline-time.monotonic()))
            finally:
                if process.poll() is None:
                    process.kill()
                code = process.wait()
            if not truncated and code not in (0, 1):
                raise ValueError(errors.decode(errors='replace') or 'Project search failed')
        return dict(matches=output[:24000].decode(errors='replace'), truncated=truncated)


def tool_result(value, success=True):
    return dict(success=success, contentItems=[dict(type='inputText', text=json.dumps(value, ensure_ascii=False))])
