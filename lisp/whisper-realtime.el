;;; whisper-realtime.el --- Decode microphone dictation in the background -*- lexical-binding: t; -*-

;;; Commentary:
;; Background decoding for the dedicated real-time dictation commands.
;; Whole-recording commands use whisper.el's original recorder instead.
;; Both paths hide drafts until completion.

;;; Code:

(require 'whisper)
(require 'json)
(require 'cl-lib)

(declare-function my/whisper--watch-recording-start "whisper-target" (&rest args))
(defvar my/whisper--target-buffer)

(defvar my/whisper--realtime nil
  "Dynamically bound while a dedicated real-time command starts recording.")

(defcustom my/whisper-realtime-step 10
  "Seconds to wait between background decoding attempts.
Full blocks and stopped capture bypass this delay."
  :type 'number :group 'whisper)

(defcustom my/whisper-realtime-window 30
  "Maximum seconds of audio in one background transcription block."
  :type 'number :group 'whisper)

(defconst my/whisper-realtime--script
  (expand-file-name "whisper-realtime.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun my/whisper-realtime--p (process)
  "Return non-nil if PROCESS decodes audio while recording."
  (and (processp process) (process-get process 'whisper-realtime)))

(defun my/whisper-realtime--release-input (process)
  "Release PROCESS's microphone state without losing its pending result."
  (unless (process-get process 'whisper-realtime-released)
    (process-put process 'whisper-realtime-released t)
    (when (eq whisper--recording-process process)
      (setq whisper--recording-process nil)
      (whisper--setup-mode-line :hide 'recording)
      (when (and (not (process-get process 'whisper-realtime-cancelled))
                 (not (process-get process 'whisper-realtime-on-release))
                 (process-live-p process))
        (setq whisper--transcribing-process process)
        (whisper--setup-mode-line :show 'transcribing)))
    (unless (process-get process 'whisper-realtime-cancelled)
      (when-let* ((release (process-get process 'whisper-realtime-on-release)))
        (funcall release process)))))

(defun my/whisper-realtime--event (process event)
  "Accumulate one background transcription EVENT from PROCESS."
  (pcase (plist-get event :event)
    ("ready"
     (process-put process 'whisper-input-pid (plist-get event :recording_pid))
     (when (eq whisper--recording-process process)
       (whisper--setup-mode-line :show 'recording)
       (my/whisper--watch-recording-start)))
    ((or "partial" "final")
     (when (buffer-live-p (process-buffer process))
       (with-current-buffer (process-buffer process)
         (let ((draft (process-get process 'whisper-realtime-draft)))
           (goto-char draft)
           (delete-region (point) (point-max))
           (insert (plist-get event :text))
           (when (equal (plist-get event :event) "final")
             (unless (bolp) (insert "\n"))
             (set-marker draft (point)))))))
    ("capture-ended" (my/whisper-realtime--release-input process))
    ("stopped" (process-put process 'whisper-realtime-complete t))
    ("error" (process-put process 'whisper-realtime-error (plist-get event :message)))))

(defun my/whisper-realtime--filter (process output)
  "Decode complete JSON events in PROCESS's OUTPUT."
  (let ((pending (concat (process-get process 'pending) output)))
    (while (string-match "\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (my/whisper-realtime--event
         process (json-parse-string line :object-type 'plist :null-object nil))))
    (process-put process 'pending pending)))

(defun my/whisper-realtime--insert-result (process _event)
  "Deliver PROCESS's completed result through Whisper's normal output handler."
  (let* ((output (process-buffer process))
         (logs (process-get process 'whisper-realtime-logs))
         (audio (process-get process 'whisper-realtime-audio))
         (target (process-get process 'whisper-realtime-target))
         (success (and (eq (process-status process) 'exit)
                       (zerop (process-exit-status process))
                       (process-get process 'whisper-realtime-complete)))
         (text (with-current-buffer output (string-trim (buffer-string)))))
    (when (eq my/whisper--target-buffer target)
      (setq my/whisper--target-buffer nil))
    (cond
     ((not success)
      (with-current-buffer logs
        (goto-char (point-max))
        (insert (format "\n%s\nRecording kept: %s\n"
                        (or (process-get process 'whisper-realtime-error)
                            "Whisper dictation did not finish") audio)))
      (display-buffer logs)
      (message "Whisper dictation failed; recording kept: %s" audio))
     ((string-empty-p text)
      (kill-buffer logs)
      (message "No speech detected; check microphone mute/input. Recording kept: %s" audio))
     (t
      (let ((whisper--stdout-buffer-name (buffer-name output))
            (whisper--marker (process-get process 'whisper-realtime-marker))
            (my/whisper--target-buffer target)
            (whisper-insert-text-at-point (process-get process 'whisper-realtime-insert)))
        (whisper--handle-transcription-output #'whisper--pre-process-text-output))
      (delete-file audio)
      (kill-buffer logs)))))

(defun my/whisper-realtime--finished (process event)
  "Release PROCESS and deliver only a completed, uncancelled transcription."
  (when (memq (process-status process) '(exit signal))
    (my/whisper-realtime--release-input process)
    (when (eq whisper--transcribing-process process)
      (setq whisper--transcribing-process nil)
      (whisper--setup-mode-line :hide 'transcribing))
    (unwind-protect
        (if (process-get process 'whisper-realtime-cancelled)
            (progn
              (delete-file (process-get process 'whisper-realtime-audio))
              (kill-buffer (process-get process 'whisper-realtime-logs)))
          (funcall (or (process-get process 'whisper-realtime-on-finish)
                       #'my/whisper-realtime--insert-result) process event))
      (dolist (property '(whisper-realtime-marker whisper-realtime-draft))
        (set-marker (process-get process property) nil))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun my/whisper-realtime--stop (process)
  "Stop PROCESS's microphone and finish decoding its remaining audio."
  (unless (process-get process 'whisper-realtime-stop-requested)
    (process-put process 'whisper-realtime-stop-requested t)
    ;; Signal the helper alone; its private GPU backend must survive the flush.
    (signal-process process 'SIGINT)
    (message "Finishing dictation…")))

(defun my/whisper-realtime--cancel (process)
  "Cancel PROCESS without delivering a partial transcription."
  (process-put process 'whisper-realtime-cancelled t)
  (when (eq my/whisper--target-buffer (process-get process 'whisper-realtime-target))
    (setq my/whisper--target-buffer nil))
  (my/whisper-realtime--release-input process)
  (when (eq whisper--transcribing-process process)
    (setq whisper--transcribing-process nil)
    (whisper--setup-mode-line :hide 'transcribing))
  (when (process-live-p process)
    (signal-process process 'SIGTERM)))

(defun my/whisper-realtime--run (original &rest args)
  "Stop an incremental microphone job, or call ORIGINAL with ARGS."
  (cond
   ((my/whisper-realtime--p whisper--recording-process)
    (my/whisper-realtime--stop whisper--recording-process))
   ((and (my/whisper-realtime--p whisper--transcribing-process)
         (process-live-p whisper--transcribing-process))
    ;; Keep whisper-run's existing cancellation prompt while letting the helper
    ;; reap its children and discard its hidden text cleanly.
    (when (yes-or-no-p "A transcribing job is already in progress, kill it?")
      (my/whisper-realtime--cancel whisper--transcribing-process)))
   (t (apply original args))))

(defun my/whisper-realtime--record-audio (original)
  "Use background decoding for local microphones; otherwise call ORIGINAL."
  (if (or (not my/whisper--realtime)
          whisper--ffmpeg-input-file
          (memq whisper-server-mode '(openai remote))
          (not (whisper--using-whispercpp-p)))
      (funcall original)
    (let* ((command (whisper-command ""))
           (server (whisper--find-whispercpp-server))
           (python (executable-find "python3"))
           (model (cadr (member "--model" command)))
           (vad (cadr (member "--vad-model" command))))
      (unless (and python model vad (file-executable-p server)
                   (file-readable-p model) (file-readable-p vad)
                   (executable-find "ffmpeg"))
        (user-error "Dictation requires Python, FFmpeg, whisper-server, and the installed Whisper/Silero models"))
      (unless (and (> my/whisper-realtime-step 0)
                   (<= my/whisper-realtime-step my/whisper-realtime-window)
                   (> my/whisper-realtime-window 3))
        (user-error "Dictation interval must be positive and at most the window (which must exceed three seconds)"))
      (let ((output (generate-new-buffer " *Whisper Dictation*"))
            (logs (generate-new-buffer " *Whisper Dictation Log*"))
            (audio (make-temp-file "whisper-dictation-" nil ".wav"))
            process)
        (condition-case error-data
            (progn
              (setq process
                    (make-process
                     :name "whisper-dictation" :buffer output :stderr logs :noquery t
                     :connection-type 'pipe :coding 'utf-8-unix
                     :command (append
                               (list python my/whisper-realtime--script
                                     "--server" server "--model" model "--vad-model" vad
                                     "--language" whisper-language
                                     "--threads" (number-to-string (or whisper-use-threads 4))
                                     "--input-format" whisper--ffmpeg-input-format
                                     "--input-device" whisper--ffmpeg-input-device
                                     "--step" (number-to-string my/whisper-realtime-step)
                                     "--window" (number-to-string my/whisper-realtime-window)
                                     "--save-audio" audio)
                               (when whisper-translate '("--translate"))
                               (when whisper-recording-timeout
                                 (list "--duration" (number-to-string whisper-recording-timeout))))
                     :filter #'my/whisper-realtime--filter
                     :sentinel #'my/whisper-realtime--finished))
              (process-put process 'whisper-realtime t)
              (process-put process 'whisper-realtime-logs logs)
              (process-put process 'whisper-realtime-audio audio)
              (process-put process 'whisper-realtime-target whisper--point-buffer)
              (process-put process 'whisper-realtime-insert whisper-insert-text-at-point)
              (process-put process 'whisper-realtime-marker
                           (with-current-buffer whisper--point-buffer (point-marker)))
              (process-put process 'whisper-realtime-draft
                           (with-current-buffer output (point-marker)))
              (setq whisper--recording-process process)
              (message "Loading local Whisper for dictation…")
              process)
          (error
           (kill-buffer output)
           (kill-buffer logs)
           (delete-file audio)
           (signal (car error-data) (cdr error-data))))))))

(advice-add 'whisper--record-audio :around #'my/whisper-realtime--record-audio)
(advice-add 'whisper-run :around #'my/whisper-realtime--run)

(provide 'whisper-realtime)
;;; whisper-realtime.el ends here
