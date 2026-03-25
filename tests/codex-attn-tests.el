;;; codex-attn-tests.el --- Regression tests for codex-attn -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for ambiguous same-cwd buffer matching.

;;; Code:

(require 'ert)

(unless (fboundp 'vterm-mode)
  (define-derived-mode vterm-mode fundamental-mode "VTerm"))

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
(require 'codex-attn)

(defmacro codex-attn-test--with-buffers (specs &rest body)
  "Create temporary vterm buffers described by SPECS, then run BODY.

Each item in SPECS is (SYMBOL NAME CWD).  SYMBOL is bound to the new buffer."
  (declare (indent 1) (debug t))
  `(let (codex-attn-test--buffers)
     (unwind-protect
         (let ,(mapcar
                (lambda (spec)
                  (let ((sym (nth 0 spec))
                        (name (nth 1 spec))
                        (cwd (nth 2 spec)))
                    `(,sym
                      (let ((buf (generate-new-buffer ,name)))
                        (push buf codex-attn-test--buffers)
                        (with-current-buffer buf
                          (setq default-directory ,cwd)
                          (vterm-mode))
                        buf))))
                specs)
           ,@body)
       (mapc (lambda (buf)
               (when (buffer-live-p buf)
                 (kill-buffer buf)))
             codex-attn-test--buffers))))

(ert-deftest codex-attn-targets-unique-cwd-buffer ()
  (codex-attn-test--with-buffers
      ((buf "*codex: repo*" "/tmp/codex-attn-test/"))
    (let ((codex-attn--pending-sessions nil)
          (codex-attn--thread->buffer (make-hash-table :test 'equal)))
      (let ((session (list :provider 'codex :thread_id "thread-1" :cwd "/tmp/codex-attn-test/")))
        (setq codex-attn--pending-sessions (list session))
        (should (codex-attn--session-targets-buffer-p session buf))
        (should (eq (codex-attn--buffer-for-thread 'codex "thread-1") buf))))))

(ert-deftest codex-attn-does-not-steal-session-when-multiple-buffers-share-cwd ()
  (codex-attn-test--with-buffers
      ((buf1 "*codex: repo*" "/tmp/codex-attn-test/")
       (buf2 "*codex: repo*" "/tmp/codex-attn-test/"))
    (let ((codex-attn--pending-sessions nil)
          (codex-attn--thread->buffer (make-hash-table :test 'equal)))
      (let ((session-1 (list :provider 'codex :thread_id "thread-1" :cwd "/tmp/codex-attn-test/"))
            (session-2 (list :provider 'codex :thread_id "thread-2" :cwd "/tmp/codex-attn-test/")))
        (setq codex-attn--pending-sessions (list session-1 session-2))
        (codex-attn--put-thread-buffer 'codex "thread-2" buf2)
        (should-not (codex-attn--session-targets-buffer-p session-1 buf2))
        (should-not (codex-attn--buffer-for-thread 'codex "thread-1"))
        (should (eq (codex-attn--buffer-for-thread 'codex "thread-2") buf2))
        (should (memq buf1 (codex-attn--buffers-for-cwd 'codex "/tmp/codex-attn-test/")))
        (should (memq buf2 (codex-attn--buffers-for-cwd 'codex "/tmp/codex-attn-test/")))))))

(ert-deftest codex-attn-ambiguous-cwd-is-not-actionable-without-mapping ()
  (codex-attn-test--with-buffers
      ((buf1 "*codex: repo*" "/tmp/codex-attn-test/")
       (buf2 "*codex: repo*" "/tmp/codex-attn-test/"))
    (let ((codex-attn--pending-sessions nil)
          (codex-attn--thread->buffer (make-hash-table :test 'equal)))
      (let ((session (list :provider 'codex :thread_id "thread-1" :cwd "/tmp/codex-attn-test/")))
        (setq codex-attn--pending-sessions (list session))
        (should-not (codex-attn--session-has-associated-buffer-p session))
        (codex-attn--put-thread-buffer 'codex "thread-1" buf1)
        (should (codex-attn--session-has-associated-buffer-p session))
        (should (eq (codex-attn--buffer-for-thread 'codex "thread-1") buf1))
        (should (buffer-live-p buf2))))))

(provide 'codex-attn-tests)

;;; codex-attn-tests.el ends here
