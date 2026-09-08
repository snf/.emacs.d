;;; whisper-target.el --- Keep Whisper output in its originating buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `whisper-run' remembers an insertion marker, but its output handling is
;; asynchronous.  This small adapter records the buffer selected when a
;; recording begins and directs output there.  A new transcription buffer is
;; used only when that target cannot accept the text.

;;; Code:

(require 'whisper)

(defvar my/whisper--target-buffer nil
  "Buffer selected when the current Whisper recording began.")

(defvar my/whisper--write-failure-tag nil
  "Dynamically bound tag used to leave a failed Whisper insertion.")

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
