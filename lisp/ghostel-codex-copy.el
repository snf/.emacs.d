;;; ghostel-codex-copy.el --- Copy Codex selections with M-w -*- lexical-binding: t; -*-

;; Codex's fullscreen mouse selection belongs to the application, not the
;; Emacs buffer.  Its copy action writes the native clipboard asynchronously.

(defvar ghostel--input-mode)
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-readonly-copy "ghostel" ())

(defun ghostel-codex-copy--clipboard ()
  "Read plain text from the graphical clipboard."
  (let ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                  (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)))))
    (when (stringp text)
      (substring-no-properties text))))

(defun ghostel-codex-copy-selection ()
  "Copy an Emacs region or Codex's mouse selection to the kill ring.
Codex also writes its selection to the system clipboard.  Wait briefly
for that asynchronous write before importing the text.  If no new copy
arrives, import the existing clipboard, including an identical repeat copy.
The native clipboard's additional formats, such as HTML, are preserved."
  (interactive)
  (cond
   ((memq ghostel--input-mode '(copy emacs))
    (ghostel-readonly-copy))
   ((use-region-p)
    (kill-ring-save (region-beginning) (region-end)))
   (t
    (unless (display-graphic-p)
      (user-error "Copying Codex's native clipboard requires graphical Emacs"))
    (let* ((before (ghostel-codex-copy--clipboard))
           (owned (gui-backend-selection-owner-p 'CLIPBOARD))
           (deadline (+ (float-time) 0.5))
           text)
      ;; Codex recognizes Super-C as copy on every platform.  Send its
      ;; CSI-u encoding directly: Ctrl-C and Ctrl-Shift-C can also quit
      ;; Codex views, and legacy encoders cannot preserve Super modifiers.
      (ghostel-send-string "\e[99;9u")
      (while (progn
               (accept-process-output nil 0.025)
               (setq text (ghostel-codex-copy--clipboard))
               (and (< (float-time) deadline)
                    (equal text before)
                    (or (not owned)
                        (gui-backend-selection-owner-p 'CLIPBOARD)))))
      (unless (and text (> (length text) 0))
        (user-error "No text received from Codex's clipboard"))
      ;; Codex already owns the clipboard.  Re-exporting via `kill-new'
      ;; would replace its rich clipboard formats with Emacs's plain text.
      (let ((interprogram-cut-function nil))
        (kill-new text))
      (message "Copied Codex clipboard text to the kill ring")))))

(define-minor-mode ghostel-codex-copy-mode
  "Use M-w for application selections in a Codex Ghostel buffer."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-w") #'ghostel-codex-copy-selection)
            map))

(defun ghostel-codex-copy-setup ()
  "Enable the copy binding in buffers created by the Codex launcher."
  (when (and (derived-mode-p 'ghostel-mode)
             (string-match-p "\\`\\*codex:" (buffer-name)))
    (ghostel-codex-copy-mode 1)))

(provide 'ghostel-codex-copy)
;;; ghostel-codex-copy.el ends here
