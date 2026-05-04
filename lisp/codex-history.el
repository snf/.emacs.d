;;; codex-history.el --- Insert previous Codex prompts -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico/Orderless-friendly picker for prompts stored by the Codex CLI.

;;; Code:

(require 'json)
(require 'subr-x)

(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function comint-send-string "comint" (process string))
(declare-function term-send-raw-string "term" (string))
(declare-function eat-term-send-string "eat" (terminal string))
(defvar eat-terminal)

(defgroup codex-history nil
  "Browse and insert previous Codex prompts."
  :group 'tools)

(defcustom codex-history-file
  (expand-file-name "history.jsonl" (or (getenv "CODEX_HOME") "~/.codex/"))
  "Codex CLI history file."
  :type 'file)

(defcustom codex-history-preview-width 180
  "Maximum visible prompt preview width in completion candidates."
  :type 'integer)

(defvar codex-history--cache nil)
(defvar codex-history--cache-key nil)

(defun codex-history--file-key ()
  "Return a cache key for `codex-history-file'."
  (when-let ((attrs (file-attributes codex-history-file)))
    (list codex-history-file (file-attribute-size attrs)
          (file-attribute-modification-time attrs))))

(defun codex-history--json-get (object key)
  "Return KEY from parsed JSON OBJECT."
  (cond
   ((hash-table-p object) (gethash key object))
   ((listp object) (alist-get key object nil nil #'equal))))

(defun codex-history--parse-line (line)
  "Parse one Codex history LINE."
  (when-let* (((not (string-blank-p line)))
              (object (ignore-errors
                        (json-parse-string line :object-type 'hash-table)))
              (text (codex-history--json-get object "text"))
              ((stringp text))
              ((not (string-empty-p text))))
    (list :text text
          :ts (codex-history--json-get object "ts")
          :session-id (codex-history--json-get object "session_id"))))

(defun codex-history--read ()
  "Return parsed Codex history entries, newest first."
  (let ((entries nil))
    (when (file-readable-p codex-history-file)
      (with-temp-buffer
        (insert-file-contents codex-history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((entry (codex-history--parse-line
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
            (when entry
              (push entry entries)))
          (forward-line 1))))
    entries))

(defun codex-history-entries (&optional refresh)
  "Return cached Codex history entries.
With REFRESH, reread `codex-history-file' unconditionally."
  (let ((key (codex-history--file-key)))
    (when (or refresh (not (equal key codex-history--cache-key)))
      (setq codex-history--cache (codex-history--read))
      (setq codex-history--cache-key key))
    codex-history--cache))

(defun codex-history--format-time (ts)
  "Format unix timestamp TS for a completion candidate."
  (if (numberp ts)
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time ts))
    "unknown time"))

(defun codex-history--one-line (text)
  "Return TEXT collapsed to one line."
  (string-trim
   (replace-regexp-in-string "[ \t\n\r]+" " " text)))

(defun codex-history--candidate (entry index)
  "Return completion candidate for ENTRY at INDEX."
  (let* ((text (plist-get entry :text))
         (session-id (or (plist-get entry :session-id) ""))
         (preview (truncate-string-to-width
                   (codex-history--one-line text)
                   codex-history-preview-width nil nil t))
         (visible (format "%s  %s"
                          (codex-history--format-time (plist-get entry :ts))
                          preview))
         ;; Keep the full prompt searchable without rendering massive rows in
         ;; Vertico. The index also makes otherwise identical prompts distinct.
         (hidden (propertize
                  (format " \t%s \t%s \t%d" text session-id index)
                  'invisible t
                  'display "")))
    (propertize (concat visible hidden) 'codex-history-entry entry)))

(defun codex-history-candidates (&optional refresh)
  "Return completion candidates for Codex history.
With REFRESH, reread `codex-history-file' unconditionally."
  (let ((index 0))
    (mapcar (lambda (entry)
              (prog1 (codex-history--candidate entry index)
                (setq index (1+ index))))
            (codex-history-entries refresh))))

(defun codex-history--insert-or-send (text)
  "Insert TEXT in the current buffer or send it to the terminal process."
  (cond
   ((derived-mode-p 'vterm-mode)
    (unless (fboundp 'vterm-send-string)
      (user-error "vterm is not loaded"))
    (vterm-send-string text t))
   ((derived-mode-p 'eat-mode)
    (unless (and (fboundp 'eat-term-send-string)
                 (bound-and-true-p eat-terminal))
      (user-error "eat terminal is not available"))
    (eat-term-send-string eat-terminal text))
   ((derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless process
        (user-error "No process for current comint buffer"))
      (comint-send-string process text)))
   ((derived-mode-p 'term-mode)
    (unless (fboundp 'term-send-raw-string)
      (user-error "term is not loaded"))
    (term-send-raw-string text))
   (buffer-read-only
    (kill-new text)
    (user-error "Buffer is read-only; copied selected Codex prompt to kill ring"))
   (t
    (insert text))))

;;;###autoload
(defun codex-history-insert-prompt (&optional refresh)
  "Select a previous Codex prompt and insert or send it at point.
With prefix argument REFRESH, reread `codex-history-file' first."
  (interactive "P")
  (let ((candidates (codex-history-candidates refresh)))
    (if candidates
        (let* ((choice (completing-read "Codex prompt: " candidates nil t))
               (entry (or (get-text-property 0 'codex-history-entry choice)
                          (get-text-property 0 'codex-history-entry
                                             (car (member choice candidates)))))
               (text (plist-get entry :text)))
          (unless text
            (user-error "Could not resolve selected Codex prompt"))
          (codex-history--insert-or-send text))
      (user-error "No Codex prompts found in %s" codex-history-file))))

(provide 'codex-history)

;;; codex-history.el ends here
