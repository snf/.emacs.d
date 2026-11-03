;;; whisper-input.el --- Select Whisper's PulseAudio source -*- lexical-binding: t; -*-

;;; Commentary:
;; Select either the host's dynamic default PulseAudio source or a specific
;; source exposed through the mounted PulseAudio/PipeWire-Pulse socket.

;;; Code:

(require 'subr-x)
(require 'json)

(defvar whisper--ffmpeg-input-format)
(defvar whisper--ffmpeg-input-device)
(defvar whisper--ffmpeg-input-file)
(defvar whisper--recording-process)

(defconst my/whisper--input-status-script
  (expand-file-name "whisper-input-status.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar my/whisper--input-monitor-process nil)
(defvar my/whisper--input-warning nil)
(defvar my/whisper--last-input-source nil)
(defvar my/whisper--input-state-file
  (locate-user-emacs-file ".cache/whisper-input.json"))

(defun my/whisper--expected-input ()
  "Return the last working microphone, including across Emacs restarts."
  (or my/whisper--last-input-source
      (when (file-readable-p my/whisper--input-state-file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents my/whisper--input-state-file)
              (setq my/whisper--last-input-source
                    (plist-get (json-parse-buffer :object-type 'plist) :source)))
          (error nil)))))

(defun my/whisper--remember-input (status)
  "Remember a healthy input from STATUS without accepting a silent fallback."
  (when-let* ((source (plist-get status :source)))
    (when (and (not (plist-get status :warning))
               (not (equal source my/whisper--last-input-source)))
      (setq my/whisper--last-input-source source)
      (make-directory (file-name-directory my/whisper--input-state-file) t)
      (with-temp-file my/whisper--input-state-file
        (insert (json-serialize (list :source source)))))))

(defun my/whisper--input-status-command (&optional once)
  "Build a status command, using ONCE for a pre-recording check."
  (append (list "uv" "run" "--no-project" "--script"
                my/whisper--input-status-script
                "--source" whisper--ffmpeg-input-device)
          (when-let* ((source (my/whisper--expected-input)))
            (list "--expected-source" source))
          (if once '("--once")
            (list "--recording-pid"
                  (number-to-string
                   (or (process-get whisper--recording-process 'whisper-input-pid)
                       (process-id whisper--recording-process)))))))

(defun my/whisper--check-input ()
  "Check microphone health before starting capture; warn without recording."
  (when (equal whisper--ffmpeg-input-format "pulse")
    (unless (executable-find "uv")
      (user-error "Microphone status unavailable: uv is missing"))
    (let ((buffer (generate-new-buffer " *Whisper Input Check*")))
      (unwind-protect
          (let* ((process (make-process
                           :name "whisper-input-check"
                           :command (my/whisper--input-status-command t)
                           :buffer buffer :noquery t :connection-type 'pipe
                           :coding 'utf-8-unix :sentinel #'ignore
                           :stderr (get-buffer-create " *Whisper Input Check Diagnostics*")))
                 (deadline (+ (float-time) 2)))
            (unwind-protect
                (progn
                  (while (and (process-live-p process) (< (float-time) deadline))
                    (accept-process-output process 0.05))
                  (unless (and (eq (process-status process) 'exit)
                               (zerop (process-exit-status process)))
                    (user-error "Microphone status unavailable; recording did not start"))
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (let* ((status (json-parse-buffer :object-type 'plist :null-object nil))
                           (warning (plist-get status :warning)))
                      (my/whisper--set-input-warning warning)
                      (when warning (user-error "%s" warning))
                      (my/whisper--remember-input status))))
              (when (process-live-p process) (delete-process process))))
        (kill-buffer buffer)))))

(defun my/whisper--set-input-warning (warning)
  "Show WARNING below the mode line without interrupting minibuffer input."
  (let ((previous my/whisper--input-warning))
    (setq my/whisper--input-warning warning)
    (cond
     ((and warning (not (active-minibuffer-window)))
      (let ((message-log-max (unless (equal warning previous) message-log-max)))
        (message "%s" (propertize warning 'face 'error))))
     ((and previous (equal (current-message) previous))
      (message nil)))))

(defun my/whisper--input-status-filter (process output)
  "Read complete status lines from PROCESS's OUTPUT."
  (internal-default-process-filter process output)
  (when (eq process my/whisper--input-monitor-process)
    (let ((pending (concat (process-get process 'pending) output)))
      (while (string-match "\n" pending)
        (let ((line (substring pending 0 (match-beginning 0))))
          (setq pending (substring pending (match-end 0)))
          (condition-case nil
              (let ((status (json-parse-string line :object-type 'plist
                                              :null-object nil)))
                (my/whisper--remember-input status)
                (my/whisper--set-input-warning (plist-get status :warning)))
            (json-parse-error nil))))
      (process-put process 'pending pending))))

(defun my/whisper--stop-input-monitor ()
  "Stop checking the microphone and clear its warning."
  (let ((process my/whisper--input-monitor-process))
    (setq my/whisper--input-monitor-process nil)
    (when (process-live-p process)
      (delete-process process)))
  (my/whisper--set-input-warning nil))

(defun my/whisper--start-input-monitor ()
  "Watch the actual PulseAudio input while Whisper records microphone audio."
  (my/whisper--stop-input-monitor)
  (when (and (equal whisper--ffmpeg-input-format "pulse")
             (not whisper--ffmpeg-input-file)
             (process-live-p whisper--recording-process))
    (if (not (executable-find "uv"))
        (my/whisper--set-input-warning "Microphone status unavailable: uv is missing")
      (let ((buffer (get-buffer-create " *Whisper Input Status*")))
        (with-current-buffer buffer (erase-buffer))
        (setq my/whisper--input-monitor-process
              (make-process
               :name "whisper-input-status"
               :command (my/whisper--input-status-command)
               :buffer buffer :connection-type 'pipe :coding 'utf-8-unix :noquery t
               :filter #'my/whisper--input-status-filter
               :sentinel
               (lambda (process _event)
                 (when (and (eq process my/whisper--input-monitor-process)
                            (memq (process-status process) '(exit signal)))
                   (setq my/whisper--input-monitor-process nil)
                   (my/whisper--set-input-warning
                    (when (process-live-p whisper--recording-process)
                      "Microphone status check stopped"))))))))))

(defun my/whisper--pulse-sources ()
  "Return plists describing PulseAudio sources reported by FFmpeg."
  (unless (executable-find "ffmpeg")
    (user-error "FFmpeg is required to enumerate PulseAudio sources"))
  (with-temp-buffer
    (let ((status
           (process-file "ffmpeg" nil (list (current-buffer) t) nil
                         "-hide_banner" "-sources" "pulse")))
      (unless (zerop status)
        (user-error "Could not query PulseAudio sources: %s"
                    (string-trim (buffer-string))))
      (goto-char (point-min))
      (let (sources)
        (while (re-search-forward
                (concat "^\\([* ]\\)[[:space:]]+"
                        "\\([^[:space:]\n]+\\)[[:space:]]+"
                        "\\[\\([^]\n]+\\)\\]")
                nil t)
          (push (list :name (match-string 2)
                      :description (match-string 3)
                      :host-default (equal (match-string 1) "*"))
                sources))
        (nreverse sources)))))

;;;###autoload
(defun my/whisper-select-input-device ()
  "Select the PulseAudio source used by Whisper.

Choosing `Host default' keeps Whisper synchronized with microphone changes
made on the host.  Choosing a named source pins Whisper to that source until
this command is run again."
  (interactive)
  (let* ((sources (my/whisper--pulse-sources))
         (choices
          (cons
           (cons "Host default — follow host microphone changes [default]"
                 "default")
           (mapcar
            (lambda (source)
              (cons
               (format "%s [%s]%s"
                       (plist-get source :description)
                       (plist-get source :name)
                       (if (plist-get source :host-default)
                           " [host default now]"
                         ""))
               (plist-get source :name)))
            sources)))
         (selection (completing-read "Whisper input: " choices nil t))
         (device (cdr (assoc selection choices))))
    (require 'whisper)
    (setq whisper--ffmpeg-input-format "pulse"
          whisper--ffmpeg-input-device device
          my/whisper--last-input-source nil)
    (when (file-exists-p my/whisper--input-state-file)
      (delete-file my/whisper--input-state-file))
    (message "Whisper PulseAudio input set to %s" device)
    device))

(provide 'whisper-input)

;;; whisper-input.el ends here
