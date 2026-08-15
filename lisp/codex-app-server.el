;;; codex-app-server.el --- Shared Codex app-server lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Start the persistent Codex/Takopi mobile stack at Emacs startup and on
;; demand, and provide a per-TUI WebSocket proxy that reports the thread id
;; created by Codex.

;;; Code:

(require 'json)
(require 'subr-x)

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
  "/home/core/.emacs.d/.cache/codex/bin/codex-0.147.0-resume-cwd"
  "Codex CLI program used by remote Ghostel sessions.

This build backports preservation of the remote cwd override when an active
TUI opens its resume picker."
  :type 'string)

(defcustom codex-app-server-python-command
  '("uv" "run" "--project" "/projects/takopi" "python")
  "Command prefix for Python programs that need Takopi's dependencies."
  :type '(repeat string))

(defconst codex-app-server--module-directory
  (file-name-directory (or load-file-name buffer-file-name)))

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
          (setq codex-app-server--start-process
                (make-process
                 :name "codex-mobile-stack-start"
                 :buffer buffer
                 :command (append codex-app-server-start-command
                                  (list "--endpoint"
                                        codex-app-server-endpoint))
                 :connection-type 'pipe
                 :noquery t
                 :sentinel #'codex-app-server--start-sentinel)))
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

(defun codex-app-server--proxy-emit-thread (process thread-id)
  (unless (equal thread-id (process-get process 'codex-thread-id))
    (process-put process 'codex-thread-id thread-id)
    (when-let ((callback (process-get process 'codex-thread-callback)))
      (funcall callback thread-id))))

(defun codex-app-server--proxy-handle-line (process line)
  (condition-case nil
      (let* ((json-object-type 'plist)
             (json-key-type 'keyword)
             (message (json-read-from-string line)))
        (pcase (plist-get message :type)
          ("ready"
           (process-put process 'codex-proxy-endpoint
                        (plist-get message :endpoint))
           (when-let ((callback (process-get process 'codex-ready-callback)))
             (process-put process 'codex-ready-callback nil)
             (funcall callback process (plist-get message :endpoint))))
          ("thread"
           (codex-app-server--proxy-emit-thread
            process (plist-get message :thread_id)))
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
