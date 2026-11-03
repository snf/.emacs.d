"""State, routing and cancellation tests for the Chatty controller."""
import asyncio
from contextlib import suppress
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]/'lisp'))
from codex_chatty.common import PhraseBuffer
from codex_chatty.controller import Controller
from codex_chatty.rpc import RPCError, discussion_config
from codex_chatty.tools import ProjectReads
from codex_chatty.whisper import Whisper


class FakeSpeech:
    def __init__(self):
        self.calls = []
        self.player = None
        self.pending = {}
        self.queue = asyncio.Queue()
        self.drain = asyncio.Event()
        self.drain.set()
    async def cancel(self, epoch): self.calls.append(('cancel', epoch))
    def begin(self, epoch): self.calls.append(('begin', epoch))
    def say(self, text, epoch): self.calls.append(('say', text, epoch))
    async def finish(self, epoch):
        self.calls.append(('finish', epoch))
        await self.drain.wait()
    async def close(self): pass


class FakeRPC:
    closed = False
    def __init__(self, active=False, fail=False):
        self.calls = []
        self.active = active
        self.fail = fail
    async def request(self, method, params, **kwargs):
        self.calls.append((method, params))
        if method == 'thread/read':
            return {'thread': {'status': {'type': 'active' if self.active else 'idle'},
                               'turns': [{'id': 'working-turn', 'status': 'inProgress'}] if self.active else []}}
        if method in ('turn/start', 'turn/steer'):
            await asyncio.sleep(.01)
            if self.fail: raise TimeoutError('Connection dropped after send')
            return {'turn': {'id': 'accepted-turn'}, 'turnId': 'working-turn'}
        return {}
    async def close(self): self.closed = True


class PhraseTests(unittest.TestCase):
    def test_stream_starts_at_first_sentence_without_repeating_final(self):
        buffer = PhraseBuffer()
        self.assertEqual(buffer.feed('You control the microphone.'), [])
        self.assertEqual(buffer.feed(' We can read files.'), ['You control the microphone.'])
        self.assertEqual(buffer.feed('', final=True), ['We can read files.'])
        self.assertEqual(buffer.feed('', final=True), [])

    def test_split_preserves_all_words_and_decimal(self):
        text = 'Use Python 3.14. '+('Keep every word intact in the discussion. '*18).strip()
        buffer = PhraseBuffer()
        pieces = []
        for char in text: pieces.extend(buffer.feed(char))
        pieces.extend(buffer.feed('', final=True))
        self.assertEqual(' '.join(pieces), text)
        self.assertEqual(pieces[0], 'Use Python 3.14.')
        self.assertTrue(all(len(p)<=240 for p in pieces))

    def test_profile_disables_inherited_connected_tools_per_thread(self):
        config = discussion_config({'mcp_servers': {'mail': {}, 'dangerous': {}}})
        self.assertFalse(config['features.apps'])
        self.assertFalse(config['features.plugins'])
        self.assertFalse(config['features.shell_tool'])
        self.assertFalse(config['mcp_servers.mail.enabled'])
        self.assertEqual(config['project_doc_max_bytes'], 0)


class ReadTests(unittest.TestCase):
    def test_reads_are_bounded_and_cannot_escape_via_symlink(self):
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as outside:
            root = Path(directory)
            (root/'example.txt').write_text('first\nsecond\nthird\n')
            reads = ProjectReads(root)
            result = reads.read({'path': 'example.txt', 'start_line': 2, 'max_lines': 1})
            self.assertEqual(result['content'], '2: second')
            (Path(outside)/'private').write_text('secret')
            (root/'escape').symlink_to(outside, target_is_directory=True)
            with self.assertRaises(ValueError): reads.read({'path': 'escape/private'})
            with self.assertRaises(ValueError): reads.read({'path': str(Path(outside)/'private')})

    def test_search_uses_argument_boundaries_and_does_not_execute_shell(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root/'file.txt').write_text('needle here\n')
            result = ProjectReads(root).search({'query': 'needle'})
            self.assertIn('file.txt:1:needle here', result['matches'])
            ProjectReads(root).search({'query': '$(touch INJECTED)'})
            self.assertFalse((root/'INJECTED').exists())

    def test_search_caps_large_results_and_reports_invalid_patterns(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for i in range(100):
                (root/f'{i}.txt').write_text(('needle '+('x'*200)+'\n')*12)
            result = ProjectReads(root).search({'query': 'needle'})
            self.assertTrue(result['truncated'])
            self.assertLessEqual(len(result['matches']), 24000)
            with self.assertRaises(ValueError): ProjectReads(root).search({'query': '['})


class ControllerTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.events = []
        self.controller = Controller(dict(working_thread_id='PINNED', cwd=self.temp.name,
                                          state_dir=self.temp.name, auto_record=False,
                                          tts={'disabled': True}),
                                     lambda e, **fields: self.events.append((e, fields)))
        self.controller.speech = FakeSpeech()
        self.controller.rpc = FakeRPC()
        self.controller.chatty = 'CHATTY'
        self.controller.epoch = 7
        self.controller.turn_epochs['discussion'] = 7
    async def asyncTearDown(self):
        self.controller.closing = True
        for task in list(self.controller.tasks): task.cancel()
        for task in self.controller.tool_calls.values(): task.cancel()
        await asyncio.gather(*self.controller.tasks, *self.controller.tool_calls.values(), return_exceptions=True)
        self.controller.log.close()
        self.temp.cleanup()

    def tool(self, call='call', name='submit_to_working_codex', arguments=None):
        return dict(id=call, method='item/tool/call', params=dict(
            threadId='CHATTY', turnId='discussion', callId=call, tool=name,
            arguments=arguments or {'instruction': 'Do the explicitly requested task.'}))

    async def test_read_does_not_submit(self):
        (Path(self.temp.name)/'read.txt').write_text('Only inspect this')
        result = await self.controller._server_request(self.tool(name='read_project_file', arguments={'path': 'read.txt'}))
        self.assertTrue(result['success'])
        self.assertEqual(self.controller.rpc.calls, [])

    async def test_initial_context_is_bounded_and_only_attached_to_first_turn(self):
        c = self.controller
        c.connected.set_result(None)
        c.context = {'last_user_message':'begin '+('x'*20000)+' end'}
        await c.utterance('Explica esto.',7)
        first = next(p for m,p in c.rpc.calls if m=='turn/start')['input'][0]['text']
        snapshot = json.loads(first.split('<working_context>\n')[1].split('\n</working_context>')[0])
        self.assertLessEqual(len(snapshot['last_user_message']),16384)
        self.assertTrue(snapshot['last_user_message'].startswith('begin '))
        self.assertTrue(snapshot['last_user_message'].endswith(' end'))
        await c.utterance('Otra pregunta.',7)
        self.assertEqual([p for m,p in c.rpc.calls if m=='turn/start'][-1]['input'][0]['text'],'Otra pregunta.')

    async def test_idle_handoff_has_fixed_target_and_no_permission_overrides(self):
        result = await self.controller.handoff({'instruction': 'Implement it.', 'threadId': 'ATTACK'}, 7)
        self.assertEqual(result['status'], 'accepted')
        payload = next(p for m,p in self.controller.rpc.calls if m=='turn/start')
        self.assertEqual(payload['threadId'], 'PINNED')
        self.assertNotIn('sandboxPolicy', payload)
        self.assertNotIn('model', payload)

    async def test_busy_handoff_steers_expected_turn(self):
        self.controller.rpc.active = True
        result = await self.controller.handoff({'instruction': 'Keep the current API.'}, 7)
        self.assertEqual(result['mode'], 'steered')
        payload = next(p for m,p in self.controller.rpc.calls if m=='turn/steer')
        self.assertEqual(payload['expectedTurnId'], 'working-turn')

    async def test_duplicate_and_concurrent_calls_send_once(self):
        await asyncio.gather(self.controller._server_request(self.tool('a')),
                             self.controller._server_request(self.tool('a')),
                             self.controller._server_request(self.tool('b')))
        self.assertEqual(sum(m=='turn/start' for m,p in self.controller.rpc.calls), 1)

    async def test_unknown_delivery_is_never_automatically_resent(self):
        self.controller.rpc.fail = True
        first = await self.controller.handoff({'instruction': 'A task'}, 7)
        again = await self.controller.handoff({'instruction': 'A task'}, 7)
        self.assertEqual(first['status'], 'unknown')
        self.assertEqual(again, first)
        self.assertIsNone(self.controller.accepted)
        self.assertEqual(sum(m=='turn/start' for m,p in self.controller.rpc.calls), 1)
        self.assertEqual(json.loads(self.controller.receipt_path.read_text())['status'], 'unknown')

    async def test_waiting_handoff_can_be_cancelled_without_delivery(self):
        self.controller.rpc.active = True
        task = asyncio.create_task(self.controller.handoff({'instruction':'Next task','delivery':'after_current'},7))
        await asyncio.sleep(.02)
        self.controller.epoch += 1
        result = await task
        self.assertIn('cancelled', result['error'])
        self.assertFalse(any(m.startswith('turn/') for m,p in self.controller.rpc.calls))

    async def test_closing_during_delivery_persists_unknown_receipt(self):
        task = asyncio.create_task(self.controller.handoff({'instruction':'Explicit task'},7))
        while not self.controller.submitting: await asyncio.sleep(0)
        task.cancel()
        with self.assertRaises(asyncio.CancelledError): await task
        receipt = json.loads(self.controller.receipt_path.read_text())
        self.assertEqual(receipt['status'], 'unknown')
        self.assertIsNone(self.controller.accepted)

    async def test_acknowledgement_closes_only_after_player_drain(self):
        self.controller.speech.drain.clear()
        await self.controller.handoff({'instruction':'A task'},7)
        await asyncio.sleep(.08)
        self.assertFalse(self.controller.closed.is_set())
        self.assertTrue(any(c[0]=='say' and c[1]=='Sent it to Codex.' for c in self.controller.speech.calls))
        self.controller.speech.drain.set()
        await asyncio.sleep(.02)
        self.assertTrue(self.controller.closed.is_set())

    async def test_cancelled_turn_and_wrong_thread_cannot_submit(self):
        self.controller.epoch = 8
        result = await self.controller._server_request(self.tool())
        self.assertFalse(result['success'])
        wrong = self.tool('other'); wrong['params']['threadId']='OTHER'
        result = await self.controller._server_request(wrong)
        self.assertFalse(result['success'])
        self.assertEqual(self.controller.rpc.calls, [])

    async def test_agent_deltas_and_completed_item_are_spoken_once(self):
        c = self.controller
        c._event(dict(method='item/agentMessage/delta',params=dict(threadId='CHATTY',turnId='discussion',itemId='answer',delta='This is the answer. ')))
        complete=dict(method='item/completed',params=dict(threadId='CHATTY',turnId='discussion',item=dict(id='answer',type='agentMessage',text='This is the answer.')))
        c._event(complete);c._event(complete)
        self.assertEqual([x[1] for x in c.speech.calls if x[0]=='say'],['This is the answer.'])
        c._event(dict(method='item/reasoning/textDelta',params=dict(threadId='CHATTY',turnId='discussion',delta='PRIVATE')))
        self.assertFalse(any('PRIVATE' in str(x) for x in c.speech.calls))
        c.epoch=8
        c._event(dict(method='item/agentMessage/delta',params=dict(threadId='CHATTY',turnId='discussion',itemId='old',delta='STALE. ')))
        self.assertFalse(any('STALE' in str(x) for x in c.speech.calls))


class WhisperTests(unittest.IsolatedAsyncioTestCase):
    async def test_capture_begins_before_backend_is_ready_and_backend_is_reused(self):
        events=[]
        whisper=Whisper({},lambda e,**kw:events.append((e,kw)),io.StringIO())
        whisper.backend_task=asyncio.get_running_loop().create_future()
        whisper.port=1;whisper.prefix='/test'
        class Process:
            pid=123
            def __init__(self):self.stdout=io.BytesIO(b'\0'*32000);self.code=None
            def poll(self):return self.code
            def terminate(self):self.code=0
            def wait(self):self.code=0;return 0
        with patch('codex_chatty.whisper.subprocess.Popen',side_effect=lambda *a,**k:Process()), \
             patch('codex_chatty.whisper.realtime.inference',return_value=[{'text':'Hola mundo','end':1}]):
            await whisper.record('first',audio_file='fixture.wav')
            self.assertTrue(any(e=='recording' for e,k in events))
            self.assertFalse(whisper.decode_task.done())
            whisper.backend_task.set_result(None)
            await whisper.decode_task
            self.assertTrue(any(e=='transcription-complete' and k['text']=='Hola mundo' for e,k in events))
            backend=whisper.backend_task
            await whisper.record('second',audio_file='fixture.wav');await whisper.decode_task
            self.assertIs(whisper.backend_task,backend)
            await whisper.close()

    async def test_unexpected_microphone_end_never_delivers_an_utterance(self):
        events=[]
        whisper=Whisper({},lambda e,**kw:events.append((e,kw)),io.StringIO())
        whisper.backend_task=asyncio.get_running_loop().create_future()
        whisper.backend_task.set_result(None);whisper.port=1;whisper.prefix='/test'
        class Process:
            pid=123
            def __init__(self):self.stdout=io.BytesIO(b'\0'*32000)
            def poll(self):return 0
            def wait(self):return 0
        with patch('codex_chatty.whisper.subprocess.Popen',side_effect=lambda *a,**k:Process()) as opened, \
             patch('codex_chatty.whisper.realtime.inference',return_value=[{'text':'Envíalo, pero no','end':1}]):
            await whisper.record('unexpected-eof')
            self.assertNotIn('--device=default', opened.call_args.args[0])
            await whisper.decode_task
            self.assertFalse(any(e=='transcription-complete' for e,k in events))
            self.assertTrue(any(e=='transcription-error' and k.get('transcript')=='Envíalo, pero no' for e,k in events))
            await whisper.close()


if __name__=='__main__': unittest.main()
