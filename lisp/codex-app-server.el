;;; codex-app-server.el --- Shared Codex app-server lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Start the persistent Codex/Takopi mobile stack at Emacs startup and on
;; demand, and provide a per-TUI WebSocket proxy that reports the thread id
;; created by Codex.

;;; Code:

(require 'json)
(require 'subr-x)

(defconst codex-app-server--module-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defgroup codex-app-server nil
  "Shared Codex app-server integration."
  :group 'tools)

(defcustom codex-app-server-endpoint "ws://127.0.0.1:4500"
  "WebSocket endpoint of the shared Codex app-server."
  :type 'string)

(defcustom codex-app-server-ready-url "http://127.0.0.1:4500/readyz"
  "HTTP readiness endpoint for `codex-app-server-endpoint'."
  :type 'string)

(defcustom codex-app-server-start-command
  '("python3"
    "/projects/takopi/scripts/mobile_stack.py"
    "start")
  "Command that starts the persistent Codex and Takopi mobile stack.

The app-server endpoint is appended as `--endpoint ENDPOINT'."
  :type '(repeat string))

(defcustom codex-app-server-startup-timeout 20
  "Seconds to wait for the app-server readiness endpoint."
  :type 'number)

(defcustom codex-app-server-poll-interval 0.25
  "Seconds between readiness probes while starting the app-server."
  :type 'number)

(defcustom codex-app-server-curl-program "curl"
  "Program used for non-blocking readiness probes."
  :type 'string)

(defcustom codex-app-server-codex-program
  (expand-file-name "../.cache/codex/npm/bin/codex"
                    codex-app-server--module-directory)
  "Emacs-managed official Codex used by the shared server and Ghostel sessions.

Install or update it with scripts/codex-install.sh.  This stable path is
independent of the global Codex on PATH and contains no release number."
  :type 'string)

(defcustom codex-app-server-python-command
  '("uv" "run" "--project" "/projects/takopi" "python")
  "Command prefix for Python programs that need Takopi's dependencies."
  :type '(repeat string))

(defcustom codex-app-server-proxy-script
  (expand-file-name "../codex-app-server-proxy.py"
                    codex-app-server--module-directory)
  "Path to the thread-binding WebSocket proxy."
  :type 'file)

(defvar codex-app-server--waiters nil)
(defvar codex-app-server--ensuring nil)
(defvar codex-app-server--start-attempted nil)
(defvar codex-app-server--probe-process nil)
(defvar codex-app-server--start-process nil)
(defvar codex-app-server--poll-timer nil)
(defvar codex-app-server--timeout-timer nil)

(defun codex-app-server--log-buffer ()
  (get-buffer-create " *codex-app-server-start*"))

(defun codex-app-server--stack-codex-command ()
  "Return the exact Codex app-server command for the mobile stack."
  (mapconcat #'shell-quote-argument
             (list codex-app-server-codex-program
                   "app-server" "--listen" codex-app-server-endpoint)
             " "))

(defun codex-app-server--cancel-timers ()
  (when (timerp codex-app-server--poll-timer)
    (cancel-timer codex-app-server--poll-timer))
  (when (timerp codex-app-server--timeout-timer)
    (cancel-timer codex-app-server--timeout-timer))
  (setq codex-app-server--poll-timer nil
        codex-app-server--timeout-timer nil))

(defun codex-app-server--complete (error-message)
  (let ((waiters (nreverse codex-app-server--waiters)))
    (codex-app-server--cancel-timers)
    (setq codex-app-server--waiters nil
          codex-app-server--ensuring nil
          codex-app-server--start-attempted nil
          codex-app-server--probe-process nil
          codex-app-server--start-process nil)
    (dolist (waiter waiters)
      (let ((callback (if error-message (cdr waiter) (car waiter))))
        (when callback
          (condition-case err
              (if error-message
                  (funcall callback error-message)
                (funcall callback))
            (error
             (message "codex-app-server callback failed: %s"
                      (error-message-string err)))))))))

(defun codex-app-server--fail-timeout ()
  (when codex-app-server--ensuring
    (codex-app-server--complete
     (format "Codex app-server did not become ready within %.1f seconds; inspect %s"
             codex-app-server-startup-timeout
             (expand-file-name "takopi/mobile-stack.log"
                               (or (getenv "XDG_STATE_HOME") "~/.local/state"))))))

(defun codex-app-server--schedule-probe ()
  (unless (timerp codex-app-server--poll-timer)
    (setq codex-app-server--poll-timer
          (run-at-time codex-app-server-poll-interval nil
                       #'codex-app-server--probe))))

(defun codex-app-server--start-sentinel (process _event)
  (when (and (eq process codex-app-server--start-process)
             (memq (process-status process) '(exit signal)))
    (setq codex-app-server--start-process nil)
    (if (zerop (process-exit-status process))
        (codex-app-server--schedule-probe)
      (codex-app-server--complete
       (format "Mobile stack failed to start (status %d); inspect %s"
               (process-exit-status process)
               (buffer-name (process-buffer process)))))))

(defun codex-app-server--start-stack ()
  (setq codex-app-server--start-attempted t)
  (if (null codex-app-server-start-command)
      (codex-app-server--complete
       "Codex app-server is down and no start command is configured")
    (condition-case err
        (let ((buffer (codex-app-server--log-buffer)))
          (with-current-buffer buffer
            (erase-buffer))
          (let ((process-environment (copy-sequence process-environment)))
            ;; mobile_stack.py propagates this to its detached supervisor, which
            ;; uses it instead of resolving a potentially stale `codex' on PATH.
            (setenv "TAKOPI_STACK_CODEX_COMMAND"
                    (codex-app-server--stack-codex-command))
            (setq codex-app-server--start-process
                  (make-process
                   :name "codex-mobile-stack-start"
                   :buffer buffer
                   :command (append codex-app-server-start-command
                                    (list "--endpoint"
                                          codex-app-server-endpoint))
                   :connection-type 'pipe
                   :noquery t
                   :sentinel #'codex-app-server--start-sentinel))))
      (error
       (codex-app-server--complete
        (format "Could not start the Codex mobile stack: %s"
                (error-message-string err)))))))

(defun codex-app-server--probe-sentinel (process _event)
  (when (and (eq process codex-app-server--probe-process)
             (memq (process-status process) '(exit signal)))
    (setq codex-app-server--probe-process nil
          codex-app-server--poll-timer nil)
    (cond
     ((zerop (process-exit-status process))
      (codex-app-server--complete nil))
     ((not codex-app-server--start-attempted)
      (codex-app-server--start-stack))
     (t
      (codex-app-server--schedule-probe)))))

(defun codex-app-server--probe ()
  (setq codex-app-server--poll-timer nil)
  (when (and codex-app-server--ensuring
             (not (process-live-p codex-app-server--probe-process)))
    (condition-case err
        (setq codex-app-server--probe-process
              (make-process
               :name "codex-app-server-ready"
               :command (list codex-app-server-curl-program
                              "--silent" "--show-error" "--fail"
                              "--max-time" "1"
                              "--output" "/dev/null"
                              codex-app-server-ready-url)
               :connection-type 'pipe
               :noquery t
               :sentinel #'codex-app-server--probe-sentinel))
      (error
       (codex-app-server--complete
        (format "Could not probe the Codex app-server: %s"
                (error-message-string err)))))))

(defun codex-app-server--display-startup-error (error-message)
  "Display ERROR-MESSAGE from mobile stack startup."
  (display-warning 'codex-app-server error-message :error))

;;;###autoload
(defun codex-app-server-ensure (ready-callback &optional error-callback start-first)
  "Call READY-CALLBACK once the shared app-server is ready.

Start the configured mobile stack after a failed readiness probe.  When
START-FIRST is non-nil, invoke the idempotent launcher before probing; this
ensures that Takopi is supervised even when an app-server is already listening.
Concurrent callers share the same start/probe attempt.  Call ERROR-CALLBACK
with a human-readable message if startup fails."
  (push (cons ready-callback error-callback) codex-app-server--waiters)
  (unless codex-app-server--ensuring
    (setq codex-app-server--ensuring t
          codex-app-server--start-attempted nil
          codex-app-server--timeout-timer
          (run-at-time codex-app-server-startup-timeout nil
                       #'codex-app-server--fail-timeout))
    (if start-first
        (codex-app-server--start-stack)
      (codex-app-server--probe))))

;;;###autoload
(defun codex-app-server-start-mobile-stack ()
  "Start the shared Codex/Takopi mobile stack if necessary.

This function is suitable for `emacs-startup-hook'."
  (interactive)
  (codex-app-server-ensure #'ignore
                           #'codex-app-server--display-startup-error
                           t))

;;;###autoload
(defun codex-app-server-restart-mobile-stack (&optional delay-seconds)
  "Restart the shared stack after DELAY-SECONDS and three idle checks.

The restart helper runs independently of the current Codex turn, which
must finish before the shared server can become idle.  With a prefix
argument, interactively read the delay in seconds."
  (interactive (list (when current-prefix-arg
                       (read-number "Restart delay in seconds: " 0))))
  (when (process-live-p (get-process "codex-mobile-stack-restart"))
    (user-error "A Codex mobile stack restart is already pending"))
  (unless (file-executable-p codex-app-server-codex-program)
    (user-error "Install the Emacs-managed Codex with scripts/codex-install.sh"))
  (make-process
   :name "codex-mobile-stack-restart"
   :buffer (get-buffer-create " *codex-mobile-stack-restart*")
   :command (append codex-app-server-python-command
                    (list (expand-file-name "../scripts/codex-restart.py"
                                            codex-app-server--module-directory)
                          "--delay-seconds" (number-to-string (or delay-seconds 0))))
   :connection-type 'pipe
   :noquery t))

(defun codex-app-server--check-thread (thread-id callback)
  "Asynchronously call CALLBACK with whether THREAD-ID is durable.
This read-only check also protects proxies started before ephemeral filtering
was added, without restarting their live TUI connections."
  (make-process
   :name "codex-thread-check"
   :command (append codex-app-server-python-command
                    (list codex-app-server-proxy-script
                          "--endpoint" codex-app-server-endpoint
                          "--check-thread" thread-id))
   :connection-type 'pipe :noquery t
   :filter #'ignore
   :sentinel (lambda (check _event)
               (when (memq (process-status check) '(exit signal))
                 (funcall callback (and (eq (process-status check) 'exit)
                                        (zerop (process-exit-status check))))))))

(defun codex-app-server--proxy-bind-thread (process thread-id)
  (unless (equal thread-id (process-get process 'codex-thread-id))
    (process-put process 'codex-thread-id thread-id)
    (when-let ((callback (process-get process 'codex-thread-callback)))
      (funcall callback thread-id))))

(defun codex-app-server--proxy-emit-thread (process thread-id)
  (if (process-get process 'codex-filters-ephemeral)
      (codex-app-server--proxy-bind-thread process thread-id)
    ;; Old Python proxies also report the temporary title-generation thread.
    ;; Keep the displayed thread until this candidate has been verified.
    (let ((sequence (1+ (or (process-get process 'codex-binding-sequence) 0))))
      (process-put process 'codex-binding-sequence sequence)
      (codex-app-server--check-thread
       thread-id
       (lambda (durable)
         (when (and durable (process-live-p process)
                    (> sequence (or (process-get process 'codex-bound-sequence) 0)))
           (process-put process 'codex-bound-sequence sequence)
           (codex-app-server--proxy-bind-thread process thread-id)))))))

(defun codex-app-server--proxy-emit-attention (process thread-id turn-id)
  "Report a completed TURN-ID for this proxy's THREAD-ID."
  (when (equal thread-id (process-get process 'codex-thread-id))
    (when-let ((callback (process-get process 'codex-attention-callback)))
      (funcall callback thread-id turn-id))))

(defun codex-app-server--proxy-handle-line (process line)
  (condition-case nil
      (let* ((json-object-type 'plist)
             (json-key-type 'keyword)
             (message (json-read-from-string line)))
        (pcase (plist-get message :type)
          ("ready"
           (process-put process 'codex-filters-ephemeral
                        (eq t (plist-get message :filters_ephemeral)))
           (process-put process 'codex-proxy-endpoint
                        (plist-get message :endpoint))
           (when-let ((callback (process-get process 'codex-ready-callback)))
             (process-put process 'codex-ready-callback nil)
             (funcall callback process (plist-get message :endpoint))))
          ("thread"
           (codex-app-server--proxy-emit-thread
            process (plist-get message :thread_id)))
          ("attention"
           (codex-app-server--proxy-emit-attention
            process
            (plist-get message :thread_id)
            (plist-get message :turn_id)))
          ("error"
           (let ((error-message (plist-get message :message)))
             (if-let ((callback
                       (process-get process 'codex-error-callback)))
                 (funcall callback error-message)
               (message "Codex TUI proxy: %s" error-message))))))
    (error
     (message "Codex TUI proxy emitted invalid data: %s" line))))

(defun codex-app-server--proxy-filter (process output)
  (let* ((pending (concat (or (process-get process 'codex-pending-output) "")
                          output))
         (lines (split-string pending "\n"))
         (tail (car (last lines))))
    (process-put process 'codex-pending-output tail)
    (dolist (line (butlast lines))
      (unless (string-empty-p line)
        (codex-app-server--proxy-handle-line process line)))))

(defun codex-app-server--proxy-sentinel (process _event)
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'codex-proxy-stopping))
             (or (not (zerop (process-exit-status process)))
                 (null (process-get process 'codex-proxy-endpoint))))
    (let ((message-text
           (format "Codex TUI proxy exited with status %d"
                   (process-exit-status process))))
      (if-let ((callback (process-get process 'codex-error-callback)))
          (funcall callback message-text)
        (message "%s" message-text)))))

;;;###autoload
(defun codex-app-server-start-proxy (ready-callback &optional error-callback)
  "Start a multi-connection single-TUI thread-binding proxy.

Call READY-CALLBACK with the proxy process and its WebSocket endpoint.  Call
ERROR-CALLBACK if the proxy exits before or during use."
  (let ((process
         (make-process
          :name (generate-new-buffer-name "codex-tui-proxy")
          :command (append codex-app-server-python-command
                           (list codex-app-server-proxy-script
                                 "--endpoint" codex-app-server-endpoint))
          :connection-type 'pipe
          :noquery t
          :filter #'codex-app-server--proxy-filter
          :sentinel #'codex-app-server--proxy-sentinel)))
    (process-put process 'codex-ready-callback ready-callback)
    (process-put process 'codex-error-callback error-callback)
    process))

;;;###autoload
(defun codex-app-server-proxy-set-thread-callback (process callback)
  "Arrange for CALLBACK to receive PROCESS's Codex thread id."
  (process-put process 'codex-thread-callback callback)
  (when-let ((thread-id (process-get process 'codex-thread-id)))
    (funcall callback thread-id)))

;;;###autoload
(defun codex-app-server-proxy-set-attention-callback (process callback)
  "Arrange for CALLBACK to receive this proxy's completed thread and turn IDs."
  (process-put process 'codex-attention-callback callback))

;;;###autoload
(defun codex-app-server-tui-command (directory &optional endpoint thread-id)
  "Return a shell command for a remote Codex TUI.

Use DIRECTORY as the explicit Codex working root and ENDPOINT or
`codex-app-server-endpoint' as the remote server.  When THREAD-ID is non-nil,
resume that thread instead of starting a new one."
  (mapconcat
   #'shell-quote-argument
   (append (list codex-app-server-codex-program "-C" directory)
           (list "--remote" (or endpoint codex-app-server-endpoint))
           (when thread-id (list "resume"))
           (when thread-id (list thread-id)))
   " "))

;;;###autoload
(defun codex-app-server-stop-proxy (process)
  "Stop PROCESS without reporting an unexpected proxy failure."
  (when (processp process)
    (process-put process 'codex-proxy-stopping t)
    (when (process-live-p process)
      (delete-process process))))

(provide 'codex-app-server)

;;; codex-app-server.el ends here
