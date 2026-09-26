;;; ghostel-cache-maintenance.el --- Limit Ghostel glyph cache lifetime -*- lexical-binding: t; -*-

;;; Commentary:
;; Ghostel's native glyph cache retains entries for discarded terminal pages.
;; Invalidate its font identity periodically so the normal renderer releases
;; that cache.  No extra timer or forced synchronized-output redraw is needed.

;;; Code:

(defgroup ghostel-cache-maintenance nil
  "Limit the lifetime of Ghostel's native glyph cache."
  :group 'terminals)

(defcustom ghostel-cache-maintenance-interval (* 15 60)
  "Seconds between glyph cache resets in each Ghostel buffer.
An expired cache is released on the next successful terminal redraw.
Hidden terminals therefore wait until they are displayed again."
  :type '(number :tag "Seconds")
  :group 'ghostel-cache-maintenance)

(defvar-local ghostel-cache-maintenance--last-reset nil
  "Time of the last successful redraw that reset the native glyph cache.
Nil requests a reset on the next successful redraw.")

(defvar ghostel--rendered-font)
(defvar ghostel--term)
(declare-function ghostel--redraw-now "ghostel" (buffer &optional force))

(defun ghostel-cache-maintenance--redraw (original term &optional full force-sync)
  "Expire the glyph cache before calling ORIGINAL with TERM, FULL, FORCE-SYNC."
  (if (not (and (derived-mode-p 'ghostel-mode)
                (boundp 'ghostel--rendered-font)
                (boundp 'ghostel--term)
                (eq term ghostel--term)))
      (funcall original term full force-sync)
    (let* ((now (float-time))
           (expired (or (null ghostel-cache-maintenance--last-reset)
                        (>= (- now ghostel-cache-maintenance--last-reset)
                            ghostel-cache-maintenance-interval))))
      (when expired
        ;; updateFontInfo frees the old cache when this identity changes.
        (setq-local ghostel--rendered-font nil))
      (let ((rendered (funcall original term full force-sync)))
        ;; A synchronized-output frame may decline the redraw.  Keep the
        ;; reset pending until the renderer actually consumes it.
        (when (and expired rendered)
          (setq ghostel-cache-maintenance--last-reset now))
        rendered))))

;;;###autoload
(defun ghostel-cache-maintenance-reset (&optional all)
  "Request a glyph cache reset for the current Ghostel terminal.
With prefix argument ALL, request resets for every Ghostel terminal.
Use the normal redraw path to preserve line-mode input and window positions.
Hidden terminals release their caches when next displayed; synchronized
output is allowed to finish before the reset takes effect."
  (interactive "P")
  (unless (or all (derived-mode-p 'ghostel-mode))
    (user-error "The current buffer is not a Ghostel terminal"))
  (let ((count 0))
    (dolist (buffer (if all (buffer-list) (list (current-buffer))))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'ghostel-mode)
                   (bound-and-true-p ghostel--term)
                   (boundp 'ghostel--rendered-font))
          (setq-local ghostel--rendered-font nil)
          (setq ghostel-cache-maintenance--last-reset nil)
          (ghostel--redraw-now buffer)
          (setq count (1+ count)))))
    (when (called-interactively-p 'interactive)
      (message "Requested glyph cache reset for %d Ghostel terminal(s)" count))
    count))

;;;###autoload
(define-minor-mode ghostel-cache-maintenance-mode
  "Periodically reset Ghostel glyph caches during normal redraws."
  :global t
  :group 'ghostel-cache-maintenance
  (if ghostel-cache-maintenance-mode
      (progn
        (unless (fboundp 'ghostel--redraw)
          (require 'ghostel))
        (advice-add 'ghostel--redraw :around
                    #'ghostel-cache-maintenance--redraw))
    (advice-remove 'ghostel--redraw #'ghostel-cache-maintenance--redraw)))

(provide 'ghostel-cache-maintenance)
;;; ghostel-cache-maintenance.el ends here
