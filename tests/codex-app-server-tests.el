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
                 '("uv" "run" "--project" "/projects/takopi" "python")))
  (should-not
   (seq-some (lambda (argument) (string-match-p "/\\.venv/" argument))
             (append codex-app-server-start-command
                     codex-app-server-python-command))))

(ert-deftest codex-app-server-proxy-parses-ready-and-thread-events ()
  (let ((process (start-process "codex-proxy-parser-test" nil "true"))
        ready thread)
    (unwind-protect
        (progn
          (process-put process 'codex-ready-callback
                       (lambda (_process endpoint) (setq ready endpoint)))
          (process-put process 'codex-thread-callback
                       (lambda (thread-id) (setq thread thread-id)))
          (codex-app-server--proxy-handle-line
           process "{\"type\":\"ready\",\"endpoint\":\"ws://127.0.0.1:1\"}")
          (codex-app-server--proxy-handle-line
           process "{\"type\":\"thread\",\"thread_id\":\"thread-1\"}")
          (should (equal ready "ws://127.0.0.1:1"))
          (should (equal thread "thread-1")))
      (when (process-live-p process)
        (delete-process process)))))

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
