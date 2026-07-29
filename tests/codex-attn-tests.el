;;; codex-attn-tests.el --- Regression tests for codex-attn -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for terminal identity, cleanup, and incremental refreshes.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; (unless (fboundp 'vterm-mode)
;;   (define-derived-mode vterm-mode fundamental-mode "VTerm"))

(unless (fboundp 'ghostel-mode)
  (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
(require 'codex-attn)

(defmacro codex-attn-test--with-buffers (specs &rest body)
  "Create temporary terminal buffers described by SPECS, then run BODY.

Each item in SPECS is (SYMBOL NAME CWD &optional MODE).  SYMBOL is bound to
the new buffer, using `ghostel-mode' unless MODE is supplied."
  (declare (indent 1) (debug t))
  `(let (codex-attn-test--buffers)
     (unwind-protect
         (let ,(mapcar
                (lambda (spec)
                  (let ((sym (nth 0 spec))
                        (name (nth 1 spec))
                        (cwd (nth 2 spec))
                        ;; (mode (or (nth 3 spec) 'vterm-mode))
                        (mode (or (nth 3 spec) 'ghostel-mode)))
                    `(,sym
                      (let ((buf (generate-new-buffer ,name)))
                        (push buf codex-attn-test--buffers)
                        (with-current-buffer buf
                          (setq default-directory ,cwd)
                          (,mode))
                        buf))))
                specs)
           ,@body)
       (mapc (lambda (buf)
               (when (buffer-live-p buf)
                 (kill-buffer buf)))
             codex-attn-test--buffers))))

(defmacro codex-attn-test--with-state (&rest body)
  "Run BODY with isolated provider state and internal indexes."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "codex-attn-test-" t))
          (codex-attn-state-dir (expand-file-name "codex/threads" root))
          (codex-attn-opencode-state-dir (expand-file-name "opencode/threads" root))
          (codex-attn-providers
           `((codex :state-dir ,codex-attn-state-dir :buffer-prefix "*codex: ")
             (opencode :state-dir ,codex-attn-opencode-state-dir :buffer-prefix "*opencode: ")))
          (codex-attn--emacs-instance-id "test-emacs")
          (codex-attn--pending-sessions nil)
          (codex-attn--actionable-session-list nil)
          (codex-attn--terminal-id->buffer (make-hash-table :test 'equal))
          (codex-attn--sessions-by-buffer (make-hash-table :test 'eq))
          (codex-attn--state-cache (make-hash-table :test 'equal))
          (codex-attn--snoozed-until (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (make-directory codex-attn-state-dir t)
           (make-directory codex-attn-opencode-state-dir t)
           ,@body)
       (delete-directory root t))))

(defun codex-attn-test--write-state (dir thread &rest fields)
  (let ((file (expand-file-name (concat thread ".json") dir)))
    (with-temp-file file
      (insert (json-encode
               (append (list :thread_id thread
                             :provider "codex"
                             :pending_since 1
                             :last_event_ts 2)
                       fields))))
    file))

(ert-deftest codex-attn-recognizes-ghostel-buffer ()
  (codex-attn-test--with-buffers
      ((buf "*codex: repo*" "/tmp/codex-attn-test/" ghostel-mode))
    (should (eq (codex-attn--buffer-provider buf) 'codex))
    (should (codex-attn--provider-terminal-buffer-p buf 'codex))))

(ert-deftest codex-attn-recognizes-opencode-ghostel-buffer ()
  (codex-attn-test--with-buffers
      ((buf "*opencode: repo*" "/tmp/codex-attn-test/" ghostel-mode))
    (should (eq (codex-attn--buffer-provider buf) 'opencode))
    (should (codex-attn--provider-terminal-buffer-p buf 'opencode))))

(ert-deftest codex-attn-public-buffer-status-api ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: public-api*" "/tmp/"))
      (puthash buf (list 'pending) codex-attn--sessions-by-buffer)
      (should (memq buf (codex-attn-buffers 'codex)))
      (should (eq (codex-attn-buffer-provider buf) 'codex))
      (should (equal (codex-attn-provider-buffer-prefix 'codex) "*codex: "))
      (should (codex-attn-buffer-needs-attention-p buf)))))

(ert-deftest codex-attn-state-change-hook-runs-after-indicator-update ()
  (let* ((calls 0)
         (codex-attn-state-change-hook
          (list (lambda () (setq calls (1+ calls))))))
    (cl-letf (((symbol-function 'codex-attn--start-blink) #'ignore)
              ((symbol-function 'codex-attn--stop-blink) #'ignore))
      (codex-attn--update-indicator))
    (should (= 1 calls))))

(ert-deftest codex-attn-ghostel-environment-has-stable-identities ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: repo*" "/tmp/"))
      (with-current-buffer buf
        (let ((terminal-id codex-attn-terminal-id))
          (should (member "CODEX_ATTN_EMACS_INSTANCE_ID=test-emacs"
                          ghostel-environment))
          (should (member (concat "CODEX_ATTN_TERMINAL_ID=" terminal-id)
                          ghostel-environment))
          (codex-attn--ghostel-setup)
          (should (equal terminal-id codex-attn-terminal-id))
          (should (= 1 (seq-count
                        (lambda (entry)
                          (string-prefix-p "CODEX_ATTN_TERMINAL_ID=" entry))
                        ghostel-environment))))))))

(ert-deftest codex-attn-terminal-id-selects-exact-buffer ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf1 "*codex: one*" "/tmp/")
         (buf2 "*codex: two*" "/tmp/"))
      (let ((session (list :provider 'codex
                           :thread_id "thread-1"
                           :cwd "/tmp/"
                           :emacs_instance_id "test-emacs"
                           :terminal_id (buffer-local-value
                                         'codex-attn-terminal-id buf2))))
        (should-not (eq buf1 (codex-attn--buffer-for-session session)))
        (should (eq buf2 (codex-attn--buffer-for-session session)))))))

(ert-deftest codex-attn-kill-buffer-cleans-owned-state ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: repo*" "/tmp/"))
      (let* ((terminal-id (buffer-local-value 'codex-attn-terminal-id buf))
             (file (codex-attn-test--write-state
                    codex-attn-state-dir "thread-1"
                    :emacs_instance_id "test-emacs"
                    :terminal_id terminal-id)))
        (codex-attn--refresh)
        (should (= 1 (length codex-attn--pending-sessions)))
        (should (eq buf (codex-attn--buffer-for-session
                         (car codex-attn--pending-sessions))))
        (kill-buffer buf)
        (should-not (file-exists-p file))
        (should-not codex-attn--pending-sessions)
        (should-not (gethash terminal-id codex-attn--terminal-id->buffer))))))

(ert-deftest codex-attn-multiple-kills-are-independent ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf1 "*codex: one*" "/tmp/")
         (buf2 "*codex: two*" "/tmp/"))
      (let* ((id1 (buffer-local-value 'codex-attn-terminal-id buf1))
             (id2 (buffer-local-value 'codex-attn-terminal-id buf2))
             (file1 (codex-attn-test--write-state
                     codex-attn-state-dir "thread-1"
                     :emacs_instance_id "test-emacs" :terminal_id id1))
             (file2 (codex-attn-test--write-state
                     codex-attn-state-dir "thread-2"
                     :emacs_instance_id "test-emacs" :terminal_id id2)))
        (codex-attn--refresh)
        (kill-buffer buf1)
        (should-not (file-exists-p file1))
        (should (file-exists-p file2))
        (should (= 1 (length codex-attn--pending-sessions)))
        (kill-buffer buf2)
        (should-not (file-exists-p file2))
        (should-not codex-attn--pending-sessions)))))

(ert-deftest codex-attn-kill-buffer-also-cleans-snoozed-state ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: repo*" "/tmp/"))
      (let* ((terminal-id (buffer-local-value 'codex-attn-terminal-id buf))
             (file (codex-attn-test--write-state
                    codex-attn-state-dir "thread-snoozed"
                    :emacs_instance_id "test-emacs"
                    :terminal_id terminal-id)))
        (codex-attn--refresh)
        (puthash file (+ (float-time) 60) codex-attn--snoozed-until)
        (codex-attn--refresh)
        (should-not codex-attn--pending-sessions)
        (kill-buffer buf)
        (should-not (file-exists-p file))
        (should (= 0 (hash-table-count codex-attn--state-cache)))))))

(ert-deftest codex-attn-late-notification-after-kill-is-orphaned ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: repo*" "/tmp/"))
      (let ((terminal-id (buffer-local-value 'codex-attn-terminal-id buf)))
        (kill-buffer buf)
        (let ((file (codex-attn-test--write-state
                     codex-attn-state-dir "thread-late"
                     :emacs_instance_id "test-emacs"
                     :terminal_id terminal-id)))
          (codex-attn--refresh)
          (should-not (file-exists-p file))
          (should-not codex-attn--pending-sessions))))))

(ert-deftest codex-attn-foreign-emacs-state-is-ignored-not-deleted ()
  (codex-attn-test--with-state
    (let ((file (codex-attn-test--write-state
                 codex-attn-state-dir "thread-foreign"
                 :emacs_instance_id "another-emacs"
                 :terminal_id "another-terminal")))
      (codex-attn--refresh)
      (should (file-exists-p file))
      (should-not codex-attn--pending-sessions)
      (should (= 1 (hash-table-count codex-attn--state-cache))))))

(ert-deftest codex-attn-state-without-identity-is-removed ()
  (codex-attn-test--with-state
    (let ((file (codex-attn-test--write-state
                 codex-attn-state-dir "thread-without-identity" :cwd "/tmp/")))
      (codex-attn--refresh)
      (should-not (file-exists-p file))
      (should-not codex-attn--pending-sessions)
      (should (= 0 (hash-table-count codex-attn--state-cache))))))

(ert-deftest codex-attn-cache-does-not-reparse-unchanged-files ()
  (codex-attn-test--with-state
    (dotimes (number 1000)
      (codex-attn-test--write-state
       codex-attn-state-dir (format "thread-%04d" number)
       :emacs_instance_id "another-emacs"
       :terminal_id (format "terminal-%04d" number)))
    (let ((reads 0)
          (original (symbol-function 'codex-attn--read-session-file)))
      (cl-letf (((symbol-function 'codex-attn--read-session-file)
                 (lambda (provider file)
                   (setq reads (1+ reads))
                   (funcall original provider file))))
        (codex-attn--refresh)
        (should (= 1000 reads))
        (setq reads 0)
        (codex-attn--refresh)
        (should (= 0 reads))
        (should (= 1000 (hash-table-count codex-attn--state-cache)))))))

(ert-deftest codex-attn-window-hook-never-reconciles-filesystem ()
  (codex-attn-test--with-state
    (let ((codex-attn-mode t))
      (cl-letf (((symbol-function 'codex-attn--sync-state-cache)
                 (lambda () (ert-fail "window hook scanned filesystem"))))
        (codex-attn--on-buffer-visibility-change)))))

(ert-deftest codex-attn-refresh-bursts-use-one-timer ()
  (let ((codex-attn--refresh-timer nil)
        (codex-attn-refresh-delay 60))
    (unwind-protect
        (progn
          (codex-attn--schedule-refresh)
          (let ((timer codex-attn--refresh-timer))
            (codex-attn--schedule-refresh)
            (should (eq timer codex-attn--refresh-timer))))
      (when codex-attn--refresh-timer
        (cancel-timer codex-attn--refresh-timer)))))

(ert-deftest codex-attn-aborted-kill-keeps-state ()
  (codex-attn-test--with-state
    (codex-attn-test--with-buffers
        ((buf "*codex: repo*" "/tmp/"))
      (let* ((terminal-id (buffer-local-value 'codex-attn-terminal-id buf))
             (file (codex-attn-test--write-state
                    codex-attn-state-dir "thread-1"
                    :emacs_instance_id "test-emacs"
                    :terminal_id terminal-id))
             (reject (lambda () nil)))
        (codex-attn--refresh)
        (with-current-buffer buf
          (add-hook 'kill-buffer-query-functions reject nil t))
        (should-not (kill-buffer buf))
        (should (buffer-live-p buf))
        (should (file-exists-p file))
        (should (= 1 (length codex-attn--pending-sessions)))
        (with-current-buffer buf
          (remove-hook 'kill-buffer-query-functions reject t))))))

(provide 'codex-attn-tests)

;;; codex-attn-tests.el ends here
