"""Opt-in tests against the real shared app-server and local GPU/audio services.

Run CODEX_CHATTY_LIVE=1 with the Takopi Python environment. All target threads
are disposable, read-only and created by these tests; no real working thread
is used. Private PulseAudio sinks prevent capturing unrelated system audio.
"""
import asyncio
from contextlib import suppress
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest
import uuid
import wave
from unittest.mock import patch

sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'lisp'))
from codex_chatty.controller import Controller
from codex_chatty.rpc import RPC, RPCError, discussion_config
from codex_chatty.tools import function, tool_result
from codex_chatty.audio import Speech, playback_command
from codex_chatty.common import CACHE, stop_process

PULSE = CACHE/'deps/root/usr/bin'
ENABLED = os.getenv('CODEX_CHATTY_LIVE') == '1'


async def until(predicate, timeout=90):
    end=time.monotonic()+timeout
    while time.monotonic()<end:
        if predicate():return
        await asyncio.sleep(.05)
    raise TimeoutError('Expected live event did not arrive')


@unittest.skipUnless(ENABLED,'Set CODEX_CHATTY_LIVE=1 for disposable-thread integration tests')
class LiveTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.temp=tempfile.TemporaryDirectory(prefix='chatty-live-')
        self.root=Path(self.temp.name)
        self.controllers=[]
        self.threads=[]
        self.hold=asyncio.Event();self.release=asyncio.Event()
        async def server_call(message):
            self.hold.set();await self.release.wait()
            return tool_result({'released':True})
        self.admin=RPC('ws://127.0.0.1:4500',lambda m:None,server_call)
        await self.admin.connect()
        cfg=await self.admin.request('config/read',{'includeLayers':False})
        self.profile=discussion_config(cfg['config'])
        self.target=await self.new_target()

    async def new_target(self,hold=False):
        params=dict(model='gpt-6.1-terra',cwd=str(self.root),ephemeral=False,
                    sandbox='read-only',approvalPolicy='never',
                    baseInstructions='This is a disposable test thread. Do not modify files. Follow only the test user request.',
                    config=self.profile)
        if hold:params['dynamicTools']=[function('wait_for_test_release','Wait for the test harness to release this turn.',{},[])]
        result=await self.admin.request('thread/start',params)
        self.threads.append(result['thread']['id'])
        return result['thread']['id']

    async def make_controller(self,**options):
        self.events=[]
        config=dict(working_thread_id=self.target,cwd=str(self.root),state_dir=str(self.root),
                    context={'last_user_message':'We are making a voice discussion mode.',
                             'last_assistant_message':'We selected Kokoro F16 with af_heart, fast native rate, and a warm Vulkan worker.'},
                    auto_record=False,tts={'disabled':True})
        config.update(options)
        c=Controller(config,lambda e,**kw:self.events.append((e,dict(kw,observed_at=time.monotonic()))))
        self.controllers.append(c)
        await c.start();await asyncio.wait_for(asyncio.shield(c.connected),60)
        return c

    async def turn(self,c,text):
        start=len(self.events)
        await c.text(text)
        await until(lambda:any(e in ('idle','closed') for e,k in self.events[start:]))
        return self.events[start:]

    async def asyncTearDown(self):
        self.release.set()
        for c in self.controllers:
            await c.close('live test completed');await c.cleanup()
        for thread in self.threads:
            with suppress(Exception):await self.admin.request('thread/archive',{'threadId':thread},timeout=5)
        await self.admin.close()
        self.temp.cleanup()

    async def test_discussion_negation_draft_and_read_do_not_touch_working_thread(self):
        (self.root/'project.txt').write_text('The test identifier is ORCHID-72. No file may be changed.\n')
        c=await self.make_controller()
        for text in ['No envíes nada todavía. ¿Qué modelo de voz elegimos?',
                     'Leé project.txt y dime el identificador de prueba. No lo mandes a Codex.',
                     'Prepará un mensaje para que Codex revise la función, pero no lo envíes.']:
            events=await self.turn(c,text)
            self.assertFalse(any(e=='handoff' for e,k in events))
        answers=' '.join(k['text'] for e,k in self.events if e=='assistant-complete')
        self.assertIn('Kokoro',answers)
        self.assertIn('ORCHID-72',answers)
        self.assertEqual((self.root/'project.txt').read_text(),'The test identifier is ORCHID-72. No file may be changed.\n')
        try:
            state=await self.admin.request('thread/turns/list',{'threadId':self.target,'limit':5})
            self.assertEqual(state['data'],[])
        except RPCError as error:
            self.assertIn('not materialized yet',str(error))  # No first user message was submitted.
        servers=await c.rpc.request('mcpServerStatus/list',{'threadId':c.chatty,'detail':'toolsAndAuthOnly'})
        self.assertFalse(any(s.get('tools') for s in servers.get('data',[])))

    async def test_explicit_handoff_is_accepted_once_then_closes(self):
        c=await self.make_controller()
        events=await self.turn(c,'Decile a Codex: responde solamente CHATTI_ACCEPTED_83, sin modificar archivos.')
        receipts=[k for e,k in events if e=='handoff' and k['status']=='accepted']
        self.assertEqual(len(receipts),1)
        self.assertEqual(receipts[0]['working_thread_id'],self.target)
        self.assertTrue(c.closed.is_set())
        self.assertEqual(sum(e=='acknowledgement' for e,k in events),1)
        self.assertLess(next(i for i,(e,k) in enumerate(events) if e=='handoff'),
                        next(i for i,(e,k) in enumerate(events) if e=='closed'))

    async def test_busy_target_is_steered_with_real_turn_receipt(self):
        self.target=await self.new_target(hold=True)
        await self.admin.request('turn/start',{'threadId':self.target,'input':[{'type':'text','text':'Call wait_for_test_release once, then reply OK.'}]})
        await asyncio.wait_for(self.hold.wait(),90)
        c=await self.make_controller()
        c.epoch=1
        receipt=await c.handoff({'instruction':'After the wait, reply STEER_ACCEPTED without modifying files.'},1)
        self.assertEqual(receipt['mode'],'steered')
        self.assertEqual(receipt['status'],'accepted')
        self.release.set()
        await asyncio.wait_for(c.closed.wait(),5)

    async def test_spoken_close_does_not_submit(self):
        c=await self.make_controller()
        events=await self.turn(c,'Cerrá esta conversación sin enviarle nada a Codex.')
        self.assertTrue(c.closed.is_set())
        self.assertFalse(any(e=='handoff' for e,k in events))

    async def test_native_web_research_is_available_in_read_only_discussion(self):
        c=await self.make_controller()
        messages=[]
        def observe(message):
            messages.append(message)
            c._event(message)
        c.rpc.event=observe
        events=await self.turn(c,'Usa la herramienta de búsqueda web para abrir https://github.com/ggml-org/whisper.cpp y dime su licencia. No envíes nada a Codex.')
        searches=[m['params']['item'] for m in messages if m.get('method')=='item/completed'
                  and m.get('params',{}).get('item',{}).get('type')=='webSearch']
        self.assertTrue(searches,'Expected an actual native web-search item')
        answers=' '.join(k['text'] for e,k in events if e=='assistant-complete')
        self.assertIn('MIT',answers)
        self.assertFalse(any(e=='handoff' for e,k in events))

    async def test_emacs_adapter_runs_real_controller_and_renders_response(self):
        repository=Path(__file__).resolve().parents[1]
        evidence=repository/'tts-samples/2026-11-03/implementation-validation'
        evidence.mkdir(exist_ok=True)
        lisp=lambda value:json.dumps(str(value),ensure_ascii=False)
        # Reproduce the live Emacs package paths without relying on a /tmp file
        # created by an earlier manual test run.
        paths=json.loads(json.loads(subprocess.check_output(
            ['emacsclient','--eval','(json-serialize (vconcat load-path))'],text=True)))
        load_paths=self.root/'emacs-load-path.el'
        load_paths.write_text(";;; -*- lexical-binding: t; -*-\n(setq load-path (append '("+
                              ' '.join(lisp(p) for p in paths if p)+") load-path))\n")
        player=playback_command({})+['--volume=0']
        script=self.root/'emacs-live-test.el'
        script.write_text(''';;; -*- lexical-binding: t; -*-
(require 'codex-chatty)
(unless (fboundp 'ghostel-mode) (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))
(let* ((codex-chatty-record-on-open nil)
       (codex-chatty-python-command (list '''+lisp(sys.executable)+'''))
       (codex-chatty-tts-options
         (let ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol))
           (json-read-from-string '''+lisp(json.dumps({'player_command':player}))+''')))
       (codex-attn-providers '((codex :buffer-prefix "*codex: ")))
       (target (generate-new-buffer "*codex: chatty-adapter-test*")))
  (unwind-protect
      (cl-letf (((symbol-function 'codex-app-server-ensure) (lambda (ready &rest _) (funcall ready)))
                ((symbol-function 'codex-chatty--whisper-config)
                 (lambda () '((server . "/tmp/whisper.cpp/build/bin/whisper-server")
                              (model . "/tmp/whisper.cpp/models/ggml-small.bin")
                              (vad_model . "/projects/transcribe/.local-asr-research/models/ggml-silero-v6.2.0.bin")
                              (input_format . "lavfi") (input_device . "anullsrc=r=16000:cl=mono:d=0.1")))))
        (with-current-buffer target
          (ghostel-mode)
          (setq-local default-directory '''+lisp(str(self.root)+'/')+''')
          (setq-local codex-attn-thread-id '''+lisp(self.target)+''')
          (setq-local codex-attn-terminal-id "chatty-adapter-test")
          (insert "We selected Kokoro F16 and af_heart for English speech.")
          (codex-chatty))
        (codex-chatty--send codex-chatty--session "text" '(text . "No envíes nada. ¿Qué voz elegimos? Responde brevemente."))
        (let* ((process (plist-get codex-chatty--session :process))
               (deadline (+ (float-time) 90))
               (transcript (plist-get codex-chatty--session :buffer)))
          (while (and (process-live-p process) (< (float-time) deadline)
                      (not (and (equal (plist-get codex-chatty--session :state) "Ready")
                                (with-current-buffer transcript
                                  (string-match-p "Chatty: " (buffer-string))))))
            (accept-process-output process 0.1))
          (with-temp-file '''+lisp(evidence/'emacs-transcript.txt')+'''
            (insert (with-current-buffer transcript (buffer-string))))
          (unless (equal (plist-get codex-chatty--session :state) "Ready")
            (error "Emacs controller did not complete: %s" (with-current-buffer transcript (buffer-string))))
          (codex-chatty-close)
          (let ((end (+ (float-time) 10)))
            (while (and (process-live-p process) (< (float-time) end))
              (accept-process-output process 0.1)))
          (when (process-live-p process) (error "Controller did not stop"))
          (unless (zerop (process-exit-status process)) (error "Controller exited unsuccessfully"))
          (princ "EMACS_CHATTY_LIVE_OK\\n")))
    (codex-chatty-close)
    (when (buffer-live-p target) (kill-buffer target))))
''')
        command=['emacs','-Q','--batch','-l',str(load_paths),
                 '-L',str(repository/'lisp'),'-l',str(script)]
        process=await asyncio.create_subprocess_exec(*command,stdout=asyncio.subprocess.PIPE,stderr=asyncio.subprocess.PIPE)
        try:
            out,err=await asyncio.wait_for(process.communicate(),110)
        except BaseException:
            process.kill();await process.wait();raise
        (evidence/'emacs-live-test.log').write_bytes(out+err)
        self.assertEqual(process.returncode,0,(out+err).decode())
        self.assertIn(b'EMACS_CHATTY_LIVE_OK',out)
        self.assertIn('Kokoro',(evidence/'emacs-transcript.txt').read_text())

    async def test_explicit_wait_is_delivered_only_after_busy_turn_finishes(self):
        self.target=await self.new_target(hold=True)
        await self.admin.request('turn/start',{'threadId':self.target,'input':[{'type':'text','text':'Call wait_for_test_release once, then reply OK.'}]})
        await asyncio.wait_for(self.hold.wait(),90)
        c=await self.make_controller();c.epoch=1
        task=asyncio.create_task(c.handoff({'instruction':'Reply AFTER_CURRENT_ACCEPTED without modifying files.','delivery':'after_current'},1))
        await until(lambda:any(e=='status' and 'Waiting' in k.get('text','') for e,k in self.events))
        self.assertIsNone(c.accepted)
        self.release.set()
        result=await asyncio.wait_for(task,90)
        self.assertEqual(result['status'],'accepted')
        self.assertEqual(result['mode'],'started')
        await asyncio.wait_for(c.closed.wait(),5)

    async def test_spanish_microphone_pipeline_and_real_playback_reuse_workers(self):
        names=[f'chatty_input_{uuid.uuid4().hex[:8]}',f'chatty_output_{uuid.uuid4().hex[:8]}']
        modules=[];capture=None
        raw_path=self.root/'output.pcm'
        recording=None
        try:
            for name in names:
                p=await asyncio.create_subprocess_exec(str(PULSE/'pactl'),'load-module','module-null-sink',
                    'sink_name='+name,stdout=asyncio.subprocess.PIPE,stderr=asyncio.subprocess.PIPE)
                out,err=await p.communicate()
                if p.returncode:raise RuntimeError(err.decode())
                modules.append(out.decode().strip())
            fixture=Path(__file__).resolve().parents[1]/'tts-samples/2026-11-03/implementation-validation/spanish-input.wav'
            if not fixture.is_file():
                fixture=self.root/'spanish.wav'
                env=os.environ.copy();deps=str(CACHE/'deps/root/usr/lib/x86_64-linux-gnu')
                env['LD_LIBRARY_PATH']=deps;env['ESPEAK_DATA_PATH']=deps
                subprocess.run([str(PULSE/'espeak-ng'),'-v','es','-s','155','-w',str(fixture),
                                'No envíes ningún mensaje. Explica la última respuesta en inglés.'],env=env,check=True)
            recording=raw_path.open('wb')
            capture=await asyncio.create_subprocess_exec(str(PULSE/'pacat'),'--record','--raw',
                '--format=s16le','--rate=24000','--channels=1','--latency-msec=20',
                '--device='+names[1]+'.monitor',stdout=recording,stderr=asyncio.subprocess.DEVNULL)
            whisper=dict(server='/tmp/whisper.cpp/build/bin/whisper-server',
                         model='/tmp/whisper.cpp/models/ggml-small.bin',
                         vad_model='/projects/transcribe/.local-asr-research/models/ggml-silero-v6.2.0.bin',
                         input_format='pulse',input_device='default',threads=4,step=2,window=30,overlap=3)
            player=playback_command({})+['--device='+names[1]]
            # Exercise real default-source semantics safely with a private source.
            with patch.dict(os.environ,{'PULSE_SOURCE':names[0]+'.monitor'}):
                c=await self.make_controller(auto_record=True,whisper=whisper,tts={'player_command':player})
            async def speak_fixture():
                capture_id=c.capture_id
                await until(lambda:any(e=='capture-ready' and k.get('capture_id')==capture_id for e,k in self.events),10)
                p=await asyncio.create_subprocess_exec(str(PULSE/'paplay'),'--device='+names[0],str(fixture))
                self.assertEqual(await p.wait(),0)
                await asyncio.sleep(.2)
                await c.command({'command':'stop_recording'})
            await speak_fixture()
            await until(lambda:any(e=='idle' for e,k in self.events),150)
            errors=[k for e,k in self.events if e in ('error','speech-error','transcription-error')]
            self.assertFalse(errors,errors)
            transcripts=[k['text'] for e,k in self.events if e=='transcription-complete']
            self.assertTrue(transcripts and 'no' in transcripts[0].lower(),transcripts)
            self.assertTrue(any(e=='assistant-complete' for e,k in self.events))
            self.assertTrue(any(e=='audio-chunk' for e,k in self.events))
            self.assertFalse(any(e=='handoff' for e,k in self.events))
            whisper_pid=c.stt.server.pid;tts_pid=c.speech.process.pid
            start=len(self.events)
            with patch.dict(os.environ,{'PULSE_SOURCE':names[0]+'.monitor'}):
                await c.start_recording()
            await speak_fixture()
            await until(lambda:any(e=='idle' for e,k in self.events[start:]),150)
            self.assertEqual(c.stt.server.pid,whisper_pid)
            self.assertEqual(c.speech.process.pid,tts_pid)
            await stop_process(capture);capture=None;recording.close();recording=None
            import array
            values=array.array('h',raw_path.read_bytes())
            self.assertGreater(max(abs(x) for x in values),500)
            evidence=Path(__file__).resolve().parents[1]/'tts-samples/2026-11-03/implementation-validation'
            evidence.mkdir(exist_ok=True)
            with wave.open(str(evidence/'full-pipeline-output.wav'),'wb') as wav:
                wav.setnchannels(1);wav.setsampwidth(2);wav.setframerate(24000);wav.writeframes(values.tobytes())
            (evidence/'full-pipeline-events.json').write_text(json.dumps(self.events,indent=2,ensure_ascii=False))
        finally:
            await stop_process(capture)
            if recording:recording.close()
            for module in reversed(modules):
                p=await asyncio.create_subprocess_exec(str(PULSE/'pactl'),'unload-module',module)
                await p.wait()


@unittest.skipUnless(ENABLED,'Set CODEX_CHATTY_LIVE=1 for local audio tests')
class AudioLiveTests(unittest.IsolatedAsyncioTestCase):
    async def test_gpu_worker_failure_is_reported_without_controller_crash(self):
        events=[]
        with tempfile.TemporaryFile() as log:
            speech=Speech({'library':'/nonexistent/chatty-test-library.so'},lambda e,**k:events.append((e,k)),log)
            await speech.start()
            try:
                with self.assertRaises(Exception):await asyncio.wait_for(asyncio.shield(speech.ready),10)
                self.assertTrue(any(e=='speech-error' for e,k in events))
            finally:await speech.close()

    async def test_real_default_audio_device_drains_and_interruption_keeps_gpu_worker(self):
        # Exercise the actual default output, muted to avoid interrupting the user.
        events=[]
        with tempfile.TemporaryFile() as log:
            command=playback_command({})+['--volume=0']
            speech=Speech({'player_command':command},lambda e,**k:events.append((e,k)),log)
            await speech.start()
            try:
                await asyncio.wait_for(asyncio.shield(speech.ready),45)
                pid=speech.process.pid
                speech.begin(1);started=time.monotonic()
                speech.say('The voice conversation is ready.',1)
                self.assertTrue(await asyncio.wait_for(speech.finish(1),30))
                self.assertGreater(time.monotonic()-started,1)
                self.assertTrue(any(e=='audio-chunk' for e,k in events))
                speech.begin(2)
                speech.say('This is an interrupted response. Do not continue reading it.',2)
                await until(lambda:any(e=='audio-chunk' and k['epoch']==2 for e,k in events))
                started=time.monotonic();await speech.cancel(3)
                self.assertLess(time.monotonic()-started,.5)
                self.assertEqual(speech.process.pid,pid)
                speech.begin(3);speech.say('The new response is ready.',3)
                self.assertTrue(await asyncio.wait_for(speech.finish(3),30))
                self.assertEqual(speech.process.pid,pid)
                self.assertFalse(any(e=='speech-error' for e,k in events),events)
            finally:await speech.close()


if __name__=='__main__':unittest.main()
