;;; codex-history-tests.el --- Tests for Codex prompt history -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)

(unless (fboundp 'ghostel-mode)
  (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'codex-history)

(ert-deftest codex-history-sends-prompt-to-ghostel ()
  (with-temp-buffer
    (ghostel-mode)
    (setq buffer-read-only t)
    (let (sent)
      (cl-letf (((symbol-function 'ghostel-send-string)
                 (lambda (text) (setq sent text))))
        (codex-history--insert-or-send "previous prompt"))
      (should (equal sent "previous prompt")))))

(provide 'codex-history-tests)

;;; codex-history-tests.el ends here
