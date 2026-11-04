;;; codex-app-server-tests.el --- Tests for shared Codex lifecycle -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'codex-app-server)

(defmacro codex-app-server-test--with-state (&rest body)
  (declare (indent 0) (debug t))
  `(let ((codex-app-server--waiters nil)
         (codex-app-server--ensuring nil)
         (codex-app-server--start-attempted nil)
         (codex-app-server--probe-process nil)
         (codex-app-server--start-process nil)
         (codex-app-server--poll-timer nil)
         (codex-app-server--timeout-timer nil))
     (unwind-protect
         (progn ,@body)
       (codex-app-server--cancel-timers))))

(ert-deftest codex-app-server-ensure-coalesces-concurrent-callers ()
  (codex-app-server-test--with-state
    (let ((probes 0)
          callbacks)
      (cl-letf (((symbol-function 'codex-app-server--probe)
                 (lambda () (setq probes (1+ probes)))))
        (codex-app-server-ensure (lambda () (push 'one callbacks)))
        (codex-app-server-ensure (lambda () (push 'two callbacks)))
        (should (= probes 1))
        (codex-app-server--complete nil)
        (should (equal callbacks '(two one)))))))

(ert-deftest codex-app-server-failure-calls-only-error-callbacks ()
  (codex-app-server-test--with-state
    (let (ready error)
      (cl-letf (((symbol-function 'codex-app-server--probe) #'ignore))
        (codex-app-server-ensure
         (lambda () (setq ready t))
         (lambda (message) (setq error message)))
        (codex-app-server--complete "simulated failure")
        (should-not ready)
        (should (equal error "simulated failure"))))))

(ert-deftest codex-app-server-start-first-invokes-mobile-launcher ()
  (codex-app-server-test--with-state
    (let ((starts 0)
          (probes 0))
      (cl-letf (((symbol-function 'codex-app-server--start-stack)
                 (lambda () (setq starts (1+ starts))))
                ((symbol-function 'codex-app-server--probe)
                 (lambda () (setq probes (1+ probes)))))
        (codex-app-server-ensure #'ignore nil t)
        (should (= starts 1))
        (should (zerop probes))))))

(ert-deftest codex-app-server-start-command-includes-shared-endpoint ()
  (codex-app-server-test--with-state
    (let ((codex-app-server-start-command '("stack" "start"))
          (codex-app-server-endpoint "ws://127.0.0.1:9876")
          captured)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured args)
                   'fake-process)))
        (codex-app-server--start-stack)
        (should (equal (plist-get captured :command)
                       '("stack" "start" "--endpoint"
                         "ws://127.0.0.1:9876")))))))

(ert-deftest codex-app-server-default-commands-do-not-require-a-venv ()
  (should (equal codex-app-server-start-command
                 '("python3" "/projects/takopi/scripts/mobile_stack.py"
                   "start")))
  (should (equal codex-app-server-python-command
                 '("uv" "run" "--isolated" "--no-project"
                   "--with" "websockets>=15.0.1" "python")))
  (should-not
   (seq-some (lambda (argument) (string-match-p "/\\.venv/" argument))
             (append codex-app-server-start-command
                     codex-app-server-python-command))))

(ert-deftest codex-app-server-launcher-and-tui-share-the-managed-executable ()
  (let ((codex-app-server-codex-program "/private npm/bin/codex")
        (codex-app-server-start-command '("stack" "start"))
        captured)
    (codex-app-server-test--with-state
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _args)
                   (setq captured (getenv "TAKOPI_STACK_CODEX_COMMAND"))
                   'fake-process)))
        (codex-app-server--start-stack)))
    (should (equal (car (split-string-shell-command captured))
                   codex-app-server-codex-program))
    (should (equal (car (split-string-shell-command
                        (codex-app-server-tui-command "/tmp")))
                   codex-app-server-codex-program))))

(ert-deftest codex-app-server-restart-does-not-launch-the-stack-directly ()
  (let ((codex-app-server-codex-program "/private/bin/codex")
        (codex-app-server-python-command '("uv" "run" "python"))
        captured)
    (cl-letf (((symbol-function 'get-process) (lambda (&rest _) nil))
              ((symbol-function 'file-executable-p) (lambda (&rest _) t))
              ((symbol-function 'make-process)
               (lambda (&rest args) (setq captured args) 'fake-process)))
      (codex-app-server-restart-mobile-stack 28800)
      (should (equal (last (plist-get captured :command) 2)
                     '("--delay-seconds" "28800")))
      (should (member (expand-file-name "../scripts/codex-restart.py"
                                       codex-app-server--module-directory)
                      (plist-get captured :command)))
      (should (plist-get captured :noquery)))))

(ert-deftest codex-app-server-proxy-parses-ready-and-thread-events ()
  (let ((process (start-process "codex-proxy-parser-test" nil "true"))
        ready thread attention)
    (unwind-protect
        (progn
          (process-put process 'codex-ready-callback
                       (lambda (_process endpoint) (setq ready endpoint)))
          (process-put process 'codex-thread-callback
                       (lambda (thread-id) (setq thread thread-id)))
          (codex-app-server--proxy-handle-line
           process "{\"type\":\"ready\",\"endpoint\":\"ws://127.0.0.1:1\",\"filters_ephemeral\":true}")
          (codex-app-server--proxy-handle-line
           process "{\"type\":\"thread\",\"thread_id\":\"thread-1\"}")
          (process-put process 'codex-attention-callback
                       (lambda (thread-id turn-id)
                         (setq attention (list thread-id turn-id))))
          (codex-app-server--proxy-handle-line
           process "{\"type\":\"attention\",\"thread_id\":\"thread-1\",\"turn_id\":\"turn-1\"}")
          (should (equal ready "ws://127.0.0.1:1"))
          (should (equal thread "thread-1"))
          (should (equal attention '("thread-1" "turn-1"))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest codex-app-server-proxy-keeps-stderr-out-of-events-and-reports-log ()
  (let* ((codex-app-server-python-command
          '("sh" "-c"
            "printf '%s\\n' 'dependency failed' >&2; printf '%s\\n' '{\"type\":\"ready\",\"endpoint\":\"ws://127.0.0.1:1\"}'; exit 7"
            "proxy-test"))
         ready failure invalid-data
         (deadline (+ (float-time) 5))
         process stderr-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest _args)
                     (when (string-match-p "invalid data" format-string)
                       (setq invalid-data t)))))
          (setq process
                (codex-app-server-start-proxy
                 (lambda (_process endpoint) (setq ready endpoint))
                 (lambda (error-message) (setq failure error-message)))
                stderr-buffer (process-get process 'codex-stderr-buffer))
          (while (and (< (float-time) deadline)
                      (or (not failure)
                          (process-live-p (get-buffer-process stderr-buffer))))
            (accept-process-output nil 0.05))
          (should (equal ready "ws://127.0.0.1:1"))
          (should-not invalid-data)
          (should (string-match-p "exited with status 7" failure))
          (should (string-match-p (regexp-quote (buffer-name stderr-buffer))
                                  failure))
          (with-current-buffer stderr-buffer
            (should (string-match-p "dependency failed" (buffer-string)))))
      (when process (codex-app-server-stop-proxy process))
      (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer)))))

(ert-deftest codex-app-server-legacy-proxy-ignores-title-thread-and-attention ()
  (let ((process (start-process "codex-legacy-proxy-test" nil "cat"))
        checks thread attention)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-app-server--check-thread)
                   (lambda (id callback) (push (cons id callback) checks))))
          (process-put process 'codex-thread-callback (lambda (id) (setq thread id)))
          (process-put process 'codex-attention-callback
                       (lambda (id turn) (setq attention (list id turn))))
          (codex-app-server--proxy-emit-thread process "main")
          (codex-app-server--proxy-emit-thread process "title")
          ;; A title check can finish before the real conversation's check.
          (funcall (cdr (assoc "title" checks)) nil)
          (funcall (cdr (assoc "main" checks)) t)
          (should (equal thread "main"))
          (codex-app-server--proxy-emit-attention process "title" "title-turn")
          (should-not attention)
          (codex-app-server--proxy-emit-attention process "main" "main-turn")
          (should (equal attention '("main" "main-turn")))
          ;; An older durable check must not undo a newer selection.
          (codex-app-server--proxy-emit-thread process "older")
          (codex-app-server--proxy-emit-thread process "newer")
          (funcall (cdr (assoc "newer" checks)) t)
          (funcall (cdr (assoc "older" checks)) t)
          (should (equal thread "newer")))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest codex-app-server-builds-new-and-resume-tui-commands ()
  (let ((codex-app-server-codex-program "/tmp/Codex CLI"))
    (should
     (equal (codex-app-server-tui-command
             "/tmp/project root" "ws://127.0.0.1:1234")
            (concat "/tmp/Codex\\ CLI -C /tmp/project\\ root "
                    "--remote ws\\://127.0.0.1\\:1234")))
    (should
     (equal (codex-app-server-tui-command
             "/tmp/project root" "ws://127.0.0.1:1234" "thread id")
            (concat "/tmp/Codex\\ CLI -C /tmp/project\\ root "
                    "--remote ws\\://127.0.0.1\\:1234 resume thread\\ id")))))

(provide 'codex-app-server-tests)

;;; codex-app-server-tests.el ends here
