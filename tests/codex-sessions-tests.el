;;; codex-sessions-tests.el --- Tests for Codex sessions sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for sidebar ordering, rendering, and navigation.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (fboundp 'ghostel-mode)
  (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'codex-attn)
(require 'codex-sessions)

(defmacro codex-sessions-test--with-buffers (specs &rest body)
  "Create temporary Ghostel buffers from SPECS, then run BODY.

Each item in SPECS is (SYMBOL NAME)."
  (declare (indent 1) (debug t))
  `(let (codex-sessions-test--buffers)
     (unwind-protect
         (let ,(mapcar
                (lambda (spec)
                  `(,(car spec)
                    (let ((buffer (generate-new-buffer ,(cadr spec))))
                      (push buffer codex-sessions-test--buffers)
                      (with-current-buffer buffer
                        (setq default-directory temporary-file-directory)
                        (ghostel-mode))
                      buffer)))
                specs)
           ,@body)
       (mapc (lambda (buffer)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer)))
             codex-sessions-test--buffers))))

(defmacro codex-sessions-test--with-state (&rest body)
  "Run BODY with isolated notifier indexes."
  (declare (indent 0) (debug t))
  `(let ((codex-attn--terminal-id->buffer (make-hash-table :test 'equal))
         (codex-attn--sessions-by-buffer (make-hash-table :test 'eq))
         (codex-attn--pending-sessions nil)
         (codex-attn--actionable-session-list nil)
         (codex-sessions-providers '(codex opencode))
         (codex-sessions--last-buffer-signature nil)
         (codex-sessions--refresh-timer nil))
     ,@body))

(ert-deftest codex-sessions-orders-attention-before-idle ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((idle "*codex: alpha*")
         (attention "*opencode: zeta*"))
      (puthash attention (list 'pending) codex-attn--sessions-by-buffer)
      (should (equal (codex-sessions--buffers) (list attention idle)))
      (should (codex-sessions--item-attention-p attention))
      (should-not (codex-sessions--item-attention-p idle))
      (should (equal (codex-sessions--display-name idle) "alpha"))
      (should (equal (codex-sessions--display-name attention) "zeta"))
      (should (eq (get-text-property
                   0 'face (codex-sessions--item-icon attention))
                  'codex-sessions-attention-face))
      (should (eq (get-text-property
                   0 'face (codex-sessions--item-icon idle))
                  'codex-sessions-idle-face)))))

(ert-deftest codex-sessions-renders-and-refreshes-renamed-buffer ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((session "*codex: original*"))
      (let ((panel (generate-new-buffer " *codex-sessions-test*")))
        (unwind-protect
            (let ((codex-sessions-buffer-name (buffer-name panel)))
              (switch-to-buffer panel)
              (codex-sessions--initialize-buffer panel)
              (should (eq major-mode 'treemacs-mode))
              (should (string-match-p
                       (regexp-quote "original")
                       (buffer-string)))
              (should-not (string-match-p
                           (regexp-quote "codex:")
                           (buffer-string)))
              (with-current-buffer session
                (rename-buffer "*codex: renamed*"))
              (codex-sessions--redraw)
              (should (string-match-p
                       (regexp-quote "renamed")
                       (buffer-string)))
              (should-not (string-match-p
                           (regexp-quote "original")
                           (buffer-string))))
          (when (buffer-live-p panel)
            (kill-buffer panel)))))))

(ert-deftest codex-sessions-visit-uses-buffer-object ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((session "*codex: visit me*"))
      (let ((panel (generate-new-buffer " *codex-sessions-visit-test*"))
            visited)
        (unwind-protect
            (progn
              (switch-to-buffer panel)
              (codex-sessions--initialize-buffer panel)
              (goto-char (point-min))
              (search-forward "visit me")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _) (setq visited buffer))))
                (codex-sessions-visit))
              (should (eq visited session)))
          (when (buffer-live-p panel)
            (kill-buffer panel)))))))

(ert-deftest codex-sessions-clearing-attention-keeps-all-off-focus-rows ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((alpha "*codex: alpha*")
         (beta "*codex: beta*")
         (zeta "*codex: zeta*"))
      (let* ((panel (generate-new-buffer " *codex-sessions-attention-test*"))
             (codex-sessions-buffer-name (buffer-name panel))
             (main-window (selected-window))
             (panel-window
              (display-buffer-in-side-window
               panel '((side . left) (slot . 0) (window-width . 30)))))
        (unwind-protect
            (progn
              (puthash zeta (list 'pending) codex-attn--sessions-by-buffer)
              (codex-sessions--initialize-buffer panel)
              (select-window main-window)
              (remhash zeta codex-attn--sessions-by-buffer)
              (codex-sessions--redraw)
              (with-current-buffer panel
                (dolist (name '("alpha" "beta" "zeta"))
                  (should (string-match-p
                           (regexp-quote name)
                           (buffer-string))))))
          (when (window-live-p panel-window)
            (delete-window panel-window))
          (when (buffer-live-p panel)
            (kill-buffer panel)))))))

(ert-deftest codex-sessions-new-attention-keeps-and-reorders-all-rows ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((alpha "*codex: alpha*")
         (beta "*codex: beta*")
         (zeta "*codex: zeta*"))
      (let* ((panel (generate-new-buffer " *codex-sessions-new-attn-test*"))
             (codex-sessions-buffer-name (buffer-name panel))
             (main-window (selected-window))
             (panel-window
              (display-buffer-in-side-window
               panel '((side . left) (slot . 0) (window-width . 30)))))
        (unwind-protect
            (progn
              (codex-sessions--initialize-buffer panel)
              (select-window main-window)
              (puthash zeta (list 'pending) codex-attn--sessions-by-buffer)
              (codex-sessions--redraw)
              (with-current-buffer panel
                (let ((text (buffer-string)))
                  (dolist (name '("alpha" "beta" "zeta"))
                    (should (string-match-p (regexp-quote name) text)))
                  (should (< (string-match (regexp-quote "zeta") text)
                             (string-match (regexp-quote "alpha") text))))))
          (when (window-live-p panel-window)
            (delete-window panel-window))
          (when (buffer-live-p panel)
            (kill-buffer panel)))))))

(ert-deftest codex-sessions-navigation-wraps-in-both-directions ()
  (codex-sessions-test--with-state
    (codex-sessions-test--with-buffers
        ((alpha "*codex: alpha*")
         (beta "*codex: beta*")
         (zeta "*codex: zeta*"))
      (let ((panel (generate-new-buffer " *codex-sessions-cycle-test*")))
        (unwind-protect
            (progn
              (switch-to-buffer panel)
              (codex-sessions--initialize-buffer panel)
              (codex-sessions-next-line)
              (should (eq (codex-sessions--buffer-at-point) alpha))
              (codex-sessions-next-line)
              (should (eq (codex-sessions--buffer-at-point) beta))
              (codex-sessions-next-line)
              (should (eq (codex-sessions--buffer-at-point) zeta))
              (codex-sessions-next-line)
              (should (eq (codex-sessions--buffer-at-point) alpha))
              (codex-sessions-previous-line)
              (should (eq (codex-sessions--buffer-at-point) zeta))
              (should (eq (lookup-key (current-local-map) (kbd "<down>"))
                          #'codex-sessions-next-line))
              (should (eq (lookup-key (current-local-map) (kbd "C-p"))
                          #'codex-sessions-previous-line)))
          (when (buffer-live-p panel)
            (kill-buffer panel)))))))

(provide 'codex-sessions-tests)

;;; codex-sessions-tests.el ends here
