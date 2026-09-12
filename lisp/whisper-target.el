;;; whisper-target.el --- Keep Whisper output in its originating buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `whisper-run' remembers an insertion marker, but its output handling is
;; asynchronous.  This small adapter records the buffer selected when a
;; recording begins and directs output there.  A new transcription buffer is
;; used only when that target cannot accept the text.

;;; Code:

(require 'whisper)

(declare-function codex-voice-cancel-dictation "codex-voice" ())

(defvar my/whisper--target-buffer nil
  "Buffer selected when the current Whisper recording began.")

(defvar my/whisper--write-failure-tag nil
  "Dynamically bound tag used to leave a failed Whisper insertion.")

(defcustom my/whisper-long-recording-delay 300
  "Seconds to wait before showing the long-recording indicator."
  :type 'number
  :group 'whisper)

(defcustom my/whisper-long-recording-blink-interval 0.7
  "Seconds between visibility changes of the long-recording indicator."
  :type 'number
  :group 'whisper)

(defvar my/whisper--long-recording-timer nil
  "Timer that enables the long-recording indicator.")

(defvar my/whisper--long-recording-blink-timer nil
  "Timer that blinks the long-recording indicator.")

(defvar my/whisper--long-recording-active nil
  "Non-nil while the recording has passed the long-recording threshold.")

(defvar my/whisper--long-recording-visible nil
  "Non-nil when the long-recording indicator is currently visible.")

(defconst my/whisper--long-recording-mode-line-entry
  '(:eval (my/whisper--long-recording-mode-line))
  "The long-recording entry installed in `global-mode-string'.")

(defun my/whisper--long-recording-mode-line ()
  "Return the long-recording status text when it should be displayed."
  (when (and my/whisper--long-recording-active
             my/whisper--long-recording-visible)
    (propertize " ● REC 5m"
                'face '(:inherit font-lock-warning-face :height 1.15))))

(defun my/whisper--ensure-long-recording-mode-line-entry ()
  "Install the long-recording status entry once."
  (unless (member my/whisper--long-recording-mode-line-entry global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  (list my/whisper--long-recording-mode-line-entry)))))

(defun my/whisper--cancel-long-recording-timers ()
  "Cancel the timers that maintain the long-recording indicator."
  (dolist (timer (list my/whisper--long-recording-timer
                       my/whisper--long-recording-blink-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq my/whisper--long-recording-timer nil
        my/whisper--long-recording-blink-timer nil))

(defun my/whisper--clear-long-recording-status ()
  "Hide the long-recording indicator and stop its timers."
  (my/whisper--cancel-long-recording-timers)
  (setq my/whisper--long-recording-active nil
        my/whisper--long-recording-visible nil)
  (force-mode-line-update t))

(defun my/whisper--blink-long-recording-status ()
  "Toggle visibility of the long-recording indicator."
  (setq my/whisper--long-recording-visible
        (not my/whisper--long-recording-visible))
  (force-mode-line-update t))

(defun my/whisper--show-long-recording-status ()
  "Start blinking the five-minute recording status indicator."
  (setq my/whisper--long-recording-timer nil
        my/whisper--long-recording-active t
        my/whisper--long-recording-visible t)
  (setq my/whisper--long-recording-blink-timer
        (run-at-time my/whisper-long-recording-blink-interval
                     my/whisper-long-recording-blink-interval
                     #'my/whisper--blink-long-recording-status))
  (force-mode-line-update t))

(defun my/whisper--recording-elapsed-seconds ()
  "Return elapsed seconds for the current local recording process, if known."
  (when (and (processp whisper--recording-process)
             (process-live-p whisper--recording-process))
    (condition-case nil
        (when-let* ((attributes
                     (process-attributes (process-id whisper--recording-process)))
                    (elapsed (alist-get 'etime attributes)))
          (float-time elapsed))
      (error nil))))

(defun my/whisper--start-long-recording-timer ()
  "Schedule the five-minute status indicator for the active recording."
  (my/whisper--clear-long-recording-status)
  (let ((remaining
         (max 0 (- my/whisper-long-recording-delay
                   (or (my/whisper--recording-elapsed-seconds) 0)))))
    (if (zerop remaining)
        (my/whisper--show-long-recording-status)
      (setq my/whisper--long-recording-timer
            (run-at-time remaining nil
                         #'my/whisper--show-long-recording-status)))))

(defun my/whisper--watch-recording-start (&rest _)
  "Schedule long-recording status after Whisper has started recording."
  (when (whisper-recording-p)
    (my/whisper--start-long-recording-timer)))

(defun my/whisper--watch-recording-mode-line (command phase)
  "Clear long-recording status when Whisper hides its recording indicator."
  (when (and (eq command :hide) (eq phase 'recording))
    (my/whisper--clear-long-recording-status)))

(defun my/whisper--target-writable-p ()
  "Return non-nil when the saved Whisper target can receive text."
  (and (buffer-live-p my/whisper--target-buffer)
       (with-current-buffer my/whisper--target-buffer
         ;; These modes send text to an underlying terminal or window and can
         ;; do so even when their displayed buffer is read-only.
         (or (not buffer-read-only)
             (memq major-mode '(ghostel-mode vterm-mode eat-mode exwm-mode))))))

(defun my/whisper--display-fallback-buffer ()
  "Put the prepared Whisper output in a new transcription buffer."
  (let ((buffer (get-buffer-create
                 (funcall whisper-transcription-buffer-name-function))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring (get-buffer whisper--stdout-buffer-name)))
      (when whisper-display-transcription-buffer
        (display-buffer buffer)
        (visual-line-mode))
      (run-hooks 'whisper-after-insert-hook))))

(defun my/whisper--insert-text-with-fallback (original text)
  "Call ORIGINAL for TEXT, escaping to the fallback on a write failure."
  (if my/whisper--write-failure-tag
      (condition-case error-data
          (funcall original text)
        (error (throw my/whisper--write-failure-tag error-data)))
    (funcall original text)))

(defun my/whisper--handle-output-in-target (original pre-processor)
  "Insert Whisper output in the saved target, with a new-buffer fallback."
  (if (not my/whisper--target-buffer)
      (funcall original pre-processor)
    (if (not (my/whisper--target-writable-p))
        ;; Avoid Whisper's insertion branch altogether when the target is
        ;; already known to be unavailable.  The original handler still runs
        ;; its output-processing hooks before it creates the fallback buffer.
        (let ((whisper-insert-text-at-point nil))
          (funcall original pre-processor))
      (let ((failure
             (catch 'my/whisper-write-failure
               (let ((my/whisper--write-failure-tag 'my/whisper-write-failure))
                 ;; The sentinel may run with the buffer selected when the
                 ;; recording was stopped.  Force Whisper's insertion branch
                 ;; here so that buffer-local settings there cannot send this
                 ;; recording to a transcription buffer.
                 (let ((whisper-insert-text-at-point t))
                   (funcall original pre-processor))
                 nil))))
        (when failure
          ;; At this point Whisper has already prepared its output and run its
          ;; transcription hooks.  Do not run those hooks a second time.
          (my/whisper--display-fallback-buffer))))))

(defun my/whisper--clear-target (&rest _)
  "Forget the target after Whisper finishes or is cancelled."
  (setq my/whisper--target-buffer nil))

(advice-add 'whisper--insert-text :around #'my/whisper--insert-text-with-fallback)
(advice-add 'whisper--handle-transcription-output :around
            #'my/whisper--handle-output-in-target)
(advice-add 'whisper--cleanup-transcription :after #'my/whisper--clear-target)
(advice-add 'whisper--record-audio :after #'my/whisper--watch-recording-start)
(advice-add 'whisper--setup-mode-line :after #'my/whisper--watch-recording-mode-line)

(my/whisper--ensure-long-recording-mode-line-entry)

;; When this file is reloaded during an existing recording, preserve the
;; original five-minute threshold instead of starting a fresh five-minute wait.
(when (whisper-recording-p)
  (my/whisper--start-long-recording-timer))

(defun my/whisper-cancel-recording ()
  "Cancel the current Whisper recording without transcribing partial audio.

When the recording belongs to `codex-voice', cancel its capture as well so
that no partial follow-up is sent to Codex."
  (interactive)
  (cond
   ((and (boundp 'codex-voice--capture)
         codex-voice--capture
         (whisper-recording-p)
         (fboundp 'codex-voice-cancel-dictation))
    (codex-voice-cancel-dictation))
   ((whisper-recording-p)
    ;; `interrupt-process' is Whisper's normal stop-and-transcribe action.
    ;; Deleting the FFmpeg process instead leaves its sentinel without a
    ;; completed recording event, so no partial audio is transcribed.
    (delete-process whisper--recording-process)
    (my/whisper--clear-target)
    (my/whisper--clear-long-recording-status)
    (message "Whisper recording cancelled"))
   (t
    (user-error "No Whisper recording is active"))))

(defun my/whisper--start-or-stop (translate)
  "Start a Whisper recording in the current buffer, or stop the active one.
When TRANSLATE is non-nil, translate its output to English."
  (let ((starting (not (or (whisper-recording-p) (whisper-transcribing-p)))))
    (if (not starting)
        (whisper-run)
      (setq my/whisper--target-buffer (current-buffer))
      ;; Whisper checks this before recording.  Let a read-only target proceed
      ;; so the output handler can place the result in its fallback buffer.
      (setq-local whisper-insert-text-at-point
                  (my/whisper--target-writable-p))
      (setq-local whisper-translate translate)
      (condition-case error-data
          (whisper-run)
        (error
         (my/whisper--clear-target)
         (signal (car error-data) (cdr error-data)))))))

(defun my/whisper-transcribe ()
  "Toggle microphone transcription in the buffer where recording began."
  (interactive)
  (my/whisper--start-or-stop nil))

(defun my/whisper-translate ()
  "Toggle microphone translation in the buffer where recording began."
  (interactive)
  (my/whisper--start-or-stop t))

(provide 'whisper-target)
;;; whisper-target.el ends here
