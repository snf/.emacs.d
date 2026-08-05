;;; whisper-input.el --- Select Whisper's PulseAudio source -*- lexical-binding: t; -*-

;;; Commentary:
;; Select either the host's dynamic default PulseAudio source or a specific
;; source exposed through the mounted PulseAudio/PipeWire-Pulse socket.

;;; Code:

(require 'subr-x)

(defvar whisper--ffmpeg-input-format)
(defvar whisper--ffmpeg-input-device)

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
          whisper--ffmpeg-input-device device)
    (message "Whisper PulseAudio input set to %s" device)
    device))

(provide 'whisper-input)

;;; whisper-input.el ends here
