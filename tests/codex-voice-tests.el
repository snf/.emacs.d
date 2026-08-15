;;; codex-voice-tests.el --- Tests for context-aware Codex voice follow-ups -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)

(unless (fboundp 'ghostel-mode)
  (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'codex-voice)

(defmacro codex-voice-test--with-target (&rest body)
  "Run BODY in an isolated Codex Ghostel buffer with saved context."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "codex-voice-test-" t))
          (codex-voice-context-dir root)
          (codex-attn-providers
           '((codex :buffer-prefix "*codex: ")))
          (buffer (generate-new-buffer "*codex: voice-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (ghostel-mode)
           (setq-local codex-attn-terminal-id "terminal-voice")
           (setq-local codex-attn-thread-id "thread-voice")
           (with-temp-file (expand-file-name "thread-voice.json" root)
             (insert
              (json-encode
               '((thread_id . "thread-voice")
                 (provider . "codex")
                 (terminal_id . "terminal-voice")
                 (last_user_message . "Which behavior do you want?")
                 (last_assistant_message . "Choose automatic or review.")))))
           ,@body)
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (delete-directory root t))))

(ert-deftest codex-voice-reads-exact-thread-context ()
  (codex-voice-test--with-target
    (let ((context (codex-voice--read-context (current-buffer))))
      (should (equal (plist-get context :thread_id) "thread-voice"))
      (should (equal (plist-get context :last_assistant_message)
                     "Choose automatic or review.")))))

(ert-deftest codex-voice-prompt-includes-conversation-and-dictation ()
  (codex-voice-test--with-target
    (let ((prompt
           (codex-voice--formatter-prompt
            (codex-voice--read-context (current-buffer))
            "Automático, pero luego podemos revisar")))
      (should (string-match-p "Choose automatic or review" prompt))
      (should (string-match-p "Which behavior do you want" prompt))
      (should (string-match-p "Autom.tico, pero luego podemos revisar" prompt))
      (should (string-match-p "there are no repeated points" prompt))
      (should (string-match-p "words that are weirdly translated" prompt))
      (should (string-match-p "Output only the finished user response" prompt)))))

(ert-deftest codex-voice-prompt-keeps-only-last-legacy-user-message ()
  (let ((prompt
         (codex-voice--formatter-prompt
          '(:input_messages ("old request" "most recent request")
            :last_assistant_message "Latest Codex response")
          "Sí")))
    (should (string-match-p "most recent request" prompt))
    (should-not (string-match-p "old request" prompt))))

(ert-deftest codex-voice-uses-luna-with-max-reasoning ()
  (let ((command (codex-voice--formatter-command)))
    (should (member "gpt-5.6-luna" command))
    (should (member "model_reasoning_effort=\"max\"" command))))

(ert-deftest codex-voice-delivers-with-bracketed-paste-and-return ()
  (codex-voice-test--with-target
    (let (pasted keys)
      (cl-letf (((symbol-function 'ghostel-paste-string)
                 (lambda (text) (setq pasted text)))
                ((symbol-function 'ghostel-send-key)
                 (lambda (key &optional _mods) (push key keys))))
        (codex-voice--deliver (current-buffer) "Polished follow-up" t))
      (should (equal pasted "Polished follow-up"))
      (should (equal keys '("return"))))))

(ert-deftest codex-voice-review-mode-does-not-submit ()
  (codex-voice-test--with-target
    (let (pasted submitted)
      (cl-letf (((symbol-function 'ghostel-paste-string)
                 (lambda (text) (setq pasted text)))
                ((symbol-function 'ghostel-send-key)
                 (lambda (&rest _) (setq submitted t))))
        (codex-voice--deliver (current-buffer) "Review me" nil))
      (should (equal pasted "Review me"))
      (should-not submitted))))

(ert-deftest codex-voice-clear-capture-releases-timer-and-target-hook ()
  (codex-voice-test--with-target
    (let ((codex-voice--capture nil)
          (codex-voice-capture-timeout 3600))
      (unwind-protect
          (let* ((capture
                  (codex-voice--install-capture
                   (current-buffer)
                   (codex-voice--read-context (current-buffer))
                   t))
                 (timer (plist-get capture :timer)))
            (should (timerp timer))
            (should
             (memq #'codex-voice--capture-target-killed kill-buffer-hook))
            (codex-voice--clear-capture)
            (should-not codex-voice--capture)
            (should-not (memq timer timer-list))
            (should-not
             (memq #'codex-voice--capture-target-killed kill-buffer-hook)))
        (codex-voice--clear-capture)))))

(ert-deftest codex-voice-capture-timeout-releases-stale-context ()
  (codex-voice-test--with-target
    (let ((codex-voice--capture nil)
          (codex-voice-capture-timeout 3600))
      (unwind-protect
          (let ((capture
                 (codex-voice--install-capture
                  (current-buffer)
                  (codex-voice--read-context (current-buffer))
                  t)))
            (codex-voice--capture-expired capture)
            (should-not codex-voice--capture))
        (codex-voice--clear-capture)))))

(ert-deftest codex-voice-target-kill-releases-capture ()
  (codex-voice-test--with-target
    (let ((codex-voice--capture nil)
          (codex-voice-capture-timeout 3600))
      (unwind-protect
          (progn
            (codex-voice--install-capture
             (current-buffer)
             (codex-voice--read-context (current-buffer))
             t)
            (codex-voice--capture-target-killed)
            (should-not codex-voice--capture)
            (should-not
             (memq #'codex-voice--capture-target-killed kill-buffer-hook)))
        (codex-voice--clear-capture)))))

(ert-deftest codex-voice-whisper-error-releases-capture ()
  (codex-voice-test--with-target
    (let ((codex-voice--capture nil)
          (codex-voice-capture-timeout 3600))
      (unwind-protect
          (let ((capture
                 (codex-voice--install-capture
                  (current-buffer)
                  (codex-voice--read-context (current-buffer))
                  t)))
            (cl-letf (((symbol-function 'whisper-run)
                       (lambda (&rest _) (error "simulated Whisper failure"))))
              (should-error (codex-voice--run-whisper-for-capture capture)
                            :type 'error))
            (should-not codex-voice--capture))
        (codex-voice--clear-capture)))))

(ert-deftest codex-voice-formatter-process-delivers-successful-output ()
  (codex-voice-test--with-target
    (let ((fake-codex (expand-file-name "fake-codex" root))
          (codex-voice--formatter-processes nil)
          pasted
          submitted)
      (with-temp-file fake-codex
        (insert "#!/bin/sh\ncat >/dev/null\nprintf 'Context-aware follow-up'\n"))
      (set-file-modes fake-codex #o700)
      (let ((codex-voice-codex-program fake-codex))
        (cl-letf (((symbol-function 'ghostel-paste-string)
                   (lambda (text) (setq pasted text)))
                  ((symbol-function 'ghostel-send-key)
                   (lambda (&rest _) (setq submitted t))))
          (let ((process
                 (codex-voice--start-formatter
                  (current-buffer)
                  (codex-voice--read-context (current-buffer))
                  "Sí, hazlo automáticamente"
                  t)))
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (accept-process-output nil 0.05))))
      (should (equal pasted "Context-aware follow-up"))
      (should submitted))))

(ert-deftest codex-voice-allows-concurrent-formatters ()
  (codex-voice-test--with-target
    (let ((fake-codex (expand-file-name "slow-fake-codex" root))
          (codex-voice--formatter-processes nil))
      (with-temp-file fake-codex
        (insert "#!/bin/sh\ncat >/dev/null\nsleep 0.2\nprintf 'done'\n"))
      (set-file-modes fake-codex #o700)
      (let ((codex-voice-codex-program fake-codex))
        (let ((first
               (codex-voice--start-formatter
                (current-buffer) (codex-voice--read-context (current-buffer))
                "first" nil))
              (second
               (codex-voice--start-formatter
                (current-buffer) (codex-voice--read-context (current-buffer))
                "second" nil)))
          (should (process-live-p first))
          (should (process-live-p second))
          (should (= (length codex-voice--formatter-processes) 2))
          (while (or (process-live-p first) (process-live-p second))
            (accept-process-output nil 0.1)))))))

(ert-deftest codex-voice-releases-microphone-before-transcription ()
  (codex-voice-test--with-target
    (let* ((fake-whisper (expand-file-name "fake-whisper" root))
           (whisper--temp-file (expand-file-name "recording.wav" root))
           (whisper--transcribing-process nil)
           (codex-voice--capture nil)
           (codex-voice--transcription-processes nil)
           dictation)
      (with-temp-file fake-whisper
        (insert "#!/bin/sh\nsleep 0.2\nprintf 'texto transcrito'\n"))
      (set-file-modes fake-whisper #o700)
      (with-temp-file whisper--temp-file
        (insert "fake audio"))
      (let* ((capture
              (codex-voice--install-capture
               (current-buffer) (codex-voice--read-context (current-buffer)) t))
             (recording
              (make-process :name "fake-recording" :command '("sh" "-c" "true")
                            :noquery t)))
        (setf (plist-get capture :process) recording)
        (process-put recording 'codex-voice-capture capture)
        (while (process-live-p recording)
          (accept-process-output recording 0.05))
        (cl-letf (((symbol-function 'whisper-command)
                   (lambda (_audio-file) (list fake-whisper)))
                  ((symbol-function 'codex-voice--start-formatter)
                   (lambda (_target _context text _submit)
                     (setq dictation text))))
          (codex-voice--recording-finished recording "finished\n")
          (should-not codex-voice--capture)
          (should-not whisper--transcribing-process)
          (let ((transcription (car codex-voice--transcription-processes)))
            (should (process-live-p transcription))
            (while (process-live-p transcription)
              (accept-process-output transcription 0.1))
            (accept-process-output nil 0.05)))
        (should (equal dictation "texto transcrito"))))))

(ert-deftest codex-voice-preserves-context-when-terminal-closes ()
  (codex-voice-test--with-target
    (let ((file (codex-voice--context-file (current-buffer))))
      (should (file-exists-p file))
      (kill-buffer (current-buffer))
      (should (file-exists-p file)))))

(provide 'codex-voice-tests)

;;; codex-voice-tests.el ends here
