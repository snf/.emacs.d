;;; whisper-target-tests.el --- Tests for Whisper target routing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'whisper-target)

(defmacro whisper-target-test--with-buffers (&rest body)
  "Run BODY with isolated Whisper buffers."
  (declare (indent 0) (debug t))
  `(let ((target (generate-new-buffer " *whisper target*"))
         (stdout (get-buffer-create whisper--stdout-buffer-name))
         (fallback "*whisper target fallback*"))
     (unwind-protect
         (progn
           (with-current-buffer stdout (erase-buffer))
           (when (get-buffer fallback) (kill-buffer fallback))
           ,@body)
       (setq my/whisper--target-buffer nil)
       (dolist (buffer (list target stdout (get-buffer fallback)))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest whisper-target-inserts-into-the-recording-buffer ()
  "Prepared output is inserted at the point saved when recording began."
  (whisper-target-test--with-buffers
    (with-current-buffer target
      (insert "Before ")
      (goto-char (point-max))
      (setq my/whisper--target-buffer target
            whisper--marker (point-marker)))
    (with-current-buffer stdout (insert "dictated text"))
    ;; This emulates stopping the recording from another buffer whose local
    ;; setting would otherwise make Whisper choose its fallback path.
    (let ((whisper-insert-text-at-point nil))
      (whisper--handle-transcription-output #'ignore))
    (with-current-buffer target
      (should (equal (buffer-string) "Before dictated text")))))

(ert-deftest whisper-target-falls-back-when-target-is-read-only ()
  "Prepared output uses a new buffer when the saved target is read-only."
  (whisper-target-test--with-buffers
    (with-current-buffer target
      (insert "Protected")
      (setq buffer-read-only t))
    (setq my/whisper--target-buffer target)
    (with-current-buffer stdout (insert "dictated text"))
    (let ((whisper-insert-text-at-point t)
          (whisper-transcription-buffer-name-function (lambda () fallback)))
      (whisper--handle-transcription-output #'ignore))
    (with-current-buffer target
      (should (equal (buffer-string) "Protected")))
    (with-current-buffer (get-buffer fallback)
      (should (equal (buffer-string) "dictated text")))))

(ert-deftest whisper-target-keeps-starting-buffer-when-stopped-elsewhere ()
  "Stopping from another buffer inserts text in the buffer where it started."
  (let ((source (generate-new-buffer " *whisper source*"))
        (stop-buffer (generate-new-buffer " *whisper stop*"))
        (stdout (get-buffer-create whisper--stdout-buffer-name))
        (recording nil))
    (unwind-protect
        (cl-letf (((symbol-function 'whisper-recording-p)
                   (lambda () recording))
                  ((symbol-function 'whisper-transcribing-p)
                   (lambda () nil))
                  ((symbol-function 'whisper-run)
                   (lambda ()
                     (if recording
                         (progn
                           (setq recording nil)
                           (with-current-buffer stdout
                             (erase-buffer)
                             (insert "dictated text"))
                           (whisper--handle-transcription-output #'ignore))
                       (setq whisper--point-buffer (current-buffer)
                             recording t)
                       (with-current-buffer whisper--point-buffer
                         (setq whisper--marker (point-marker)))))))
          (with-current-buffer source
            (insert "Source: ")
            (my/whisper-transcribe))
          (with-current-buffer stop-buffer
            ;; Emulate a buffer-local setting that previously selected the
            ;; transcription-buffer path when the recording was stopped.
            (setq-local whisper-insert-text-at-point nil)
            (my/whisper-transcribe))
          (with-current-buffer source
            (should (equal (buffer-string) "Source: dictated text"))))
      (setq my/whisper--target-buffer nil)
      (dolist (buffer (list source stop-buffer stdout))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest whisper-target-long-recording-status-starts-after-threshold ()
  "The five-minute status starts hidden, then blinks until cleared."
  (let ((my/whisper-long-recording-delay 300)
        (my/whisper-long-recording-blink-interval 60))
    (unwind-protect
        (progn
          (my/whisper--start-long-recording-timer)
          (should (timerp my/whisper--long-recording-timer))
          (should-not my/whisper--long-recording-active)
          ;; Invoke the timer callback directly instead of waiting five minutes.
          (cancel-timer my/whisper--long-recording-timer)
          (my/whisper--show-long-recording-status)
          (should my/whisper--long-recording-active)
          (should (string-match-p "REC 5m"
                                  (my/whisper--long-recording-mode-line)))
          (my/whisper--blink-long-recording-status)
          (should-not (my/whisper--long-recording-mode-line)))
      (my/whisper--clear-long-recording-status))))

(ert-deftest whisper-target-recording-hooks-manage-long-status ()
  "Whisper's recording lifecycle starts and clears the five-minute status."
  (let (timer-started)
    (cl-letf (((symbol-function 'whisper-recording-p) (lambda () t))
              ((symbol-function 'my/whisper--start-long-recording-timer)
               (lambda () (setq timer-started t))))
      (my/whisper--watch-recording-start)
      (should timer-started))
    (unwind-protect
        (progn
          (my/whisper--show-long-recording-status)
          (my/whisper--watch-recording-mode-line :hide 'recording)
          (should-not my/whisper--long-recording-active))
      (my/whisper--clear-long-recording-status))))

(ert-deftest whisper-target-cancel-recording-does-not-transcribe ()
  "Cancellation deletes the recording process instead of interrupting it."
  (let ((target (generate-new-buffer " *whisper cancel*"))
        (whisper--recording-process 'recording-process)
        deleted)
    (unwind-protect
        (let ((my/whisper--target-buffer target))
          (cl-letf (((symbol-function 'whisper-recording-p) (lambda () t))
                    ((symbol-function 'delete-process)
                     (lambda (process) (setq deleted process))))
            (my/whisper-cancel-recording)
            (should (eq deleted 'recording-process))
            (should-not my/whisper--target-buffer)))
      (when (buffer-live-p target)
        (kill-buffer target)))))

;;; whisper-target-tests.el ends here
