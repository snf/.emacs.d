;;; codex-chatty-tests.el --- Adapter state tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
(require 'codex-chatty)

(defmacro codex-chatty-test--session (&rest body)
  (declare (indent 0))
  `(let* ((buffer (generate-new-buffer " *chatty-test*"))
          (process (make-pipe-process :name "chatty-test" :buffer nil :noquery t))
          (session (list :id "test-session" :buffer buffer :process process :state "Opening"))
          (codex-chatty--session session))
     (unwind-protect
         (progn
           (with-current-buffer buffer (codex-chatty-mode))
           (process-put process 'codex-chatty-session session)
           ,@body)
       (delete-process process)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest codex-chatty-handles-split-json-events ()
  (codex-chatty-test--session
    (codex-chatty--filter process "{\"event\":\"record")
    (should (equal (plist-get session :state) "Opening"))
    (codex-chatty--filter process "ing\",\"session_id\":\"test-session\"}\n")
    (should (equal (plist-get session :state) "Opening microphone"))
    (codex-chatty--event process '(:event "capture-ready" :session_id "test-session"))
    (should (equal (plist-get session :state) "Listening"))))

(ert-deftest codex-chatty-does-not-repeat-completed-text ()
  (codex-chatty-test--session
    (codex-chatty--event process '(:event "assistant-delta" :session_id "test-session" :item_id "one" :text "Answer."))
    (codex-chatty--event process '(:event "assistant-complete" :session_id "test-session" :item_id "one" :text "Answer."))
    (with-current-buffer buffer (should (equal (buffer-string) "\nChatty: Answer.")))))

(ert-deftest codex-chatty-ignores-events-from-an-old-session ()
  (codex-chatty-test--session
    (let ((codex-chatty--session (list :id "new-session")))
      (codex-chatty--event process '(:event "assistant-delta" :session_id "test-session" :item_id "old" :text "STALE")))
    (with-current-buffer buffer (should (string-empty-p (buffer-string))))))

(ert-deftest codex-chatty-stopping-a-recording-does-not-close-the-session ()
  (codex-chatty-test--session
    (setf (plist-get session :state) "Listening")
    (let (sent)
      (cl-letf (((symbol-function 'codex-chatty--send)
                 (lambda (_session command &rest _) (setq sent command)))
                ((symbol-function 'codex-voice--thread-id) (lambda (&rest _) nil)))
        (codex-chatty))
      (should (equal sent "stop_recording"))
      (should-not (plist-get session :closed))
      (should (equal (plist-get session :state) "Transcribing")))))

(ert-deftest codex-chatty-cannot-retarget-an-active-session ()
  (codex-chatty-test--session
    (setf (plist-get session :thread) "original")
    (cl-letf (((symbol-function 'codex-voice--thread-id) (lambda (&rest _) "another")))
      (should-error (codex-chatty) :type 'user-error))
    (should (equal (plist-get session :thread) "original"))))

(ert-deftest codex-chatty-close-only-sends-close ()
  (codex-chatty-test--session
    (let (commands)
      (cl-letf (((symbol-function 'codex-chatty--send)
                 (lambda (_session command &rest _) (push command commands))))
        (codex-chatty-close))
      (should (equal commands '("close"))))))

(ert-deftest codex-chatty-handoff-acceptance-does-not-itself-close-transcript ()
  (codex-chatty-test--session
    (codex-chatty--event process '(:event "handoff" :session_id "test-session" :status "accepted"))
    (should-not (plist-get session :closed))
    (codex-chatty--event process '(:event "closed" :session_id "test-session" :reason "handoff accepted"))
    (should (plist-get session :closed))))

(ert-deftest codex-chatty-microphone-owner-guard-is-inactive-outside-chatty ()
  (let ((codex-chatty--session nil))
    (should-not (codex-chatty--guard-other-recording))))

(ert-deftest codex-chatty-guards-and-can-stop-the-microphone-during-startup ()
  (codex-chatty-test--session
    (codex-chatty--event process '(:event "recording" :session_id "test-session"))
    (should-error (codex-chatty--guard-other-recording) :type 'user-error)
    (let (sent)
      (cl-letf (((symbol-function 'codex-chatty--send)
                 (lambda (_session command &rest _) (setq sent command)))
                ((symbol-function 'codex-voice--thread-id) (lambda (&rest _) nil)))
        (codex-chatty))
      (should (equal sent "stop_recording")))))

(provide 'codex-chatty-tests)
