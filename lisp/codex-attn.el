;;; codex-attn.el --- Codex attention indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; Modeline indicator and session jumping for pending Codex turns.

;;; Code:

;; Codex attention indicator (multi-session)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'filenotify)

(defgroup codex-attn nil
  "Codex attention indicator."
  :group 'tools)

(defcustom codex-attn-state-dir
  (expand-file-name "codex/threads" (or (getenv "XDG_CACHE_HOME") "~/.cache/"))
  "Directory where Codex notify writes per-thread state files."
  :type 'directory)

(defcustom codex-attn-blink-interval 0.6
  "Seconds between blink frames."
  :type 'number)

(defcustom codex-attn-poll-interval 2.0
  "Polling interval in seconds when filesystem watch is unavailable."
  :type 'number)

(defcustom codex-attn-queue-ttl 120
  "Seconds before queued Codex buffer candidates expire."
  :type 'number)

(defcustom codex-attn-modeline-text "Cdx!"
  "Modeline text when Codex needs attention."
  :type 'string)

(defface codex-attn-modeline-idle-face
  '((t :inherit mode-line :weight bold))
  "Face for Codex modeline indicator when pending but blink-off.")

(defface codex-attn-modeline-alert-face
  '((t :inherit mode-line :weight bold :foreground "white" :background "firebrick3"))
  "Face for Codex modeline indicator when pending and blink-on.")

(defvar codex-attn--pending-sessions nil)
(defvar codex-attn--thread->buffer (make-hash-table :test 'equal))
(defvar codex-attn--pending-buffer-queue nil)
(defvar codex-attn--watch nil)
(defvar codex-attn--poll-timer nil)
(defvar codex-attn--blink-timer nil)
(defvar codex-attn--blink-on nil)

(defvar codex-attn--modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'codex-attn-jump-most-recent)
    map))

(defconst codex-attn--modeline-entry '(:eval (codex-attn--modeline)))

(defun codex-attn--ensure-dir ()
  (unless (file-directory-p codex-attn-state-dir)
    (make-directory codex-attn-state-dir t)))

(defun codex-attn--normalize-dir (dir)
  (when (and dir (stringp dir))
    (ignore-errors (file-name-as-directory (file-truename dir)))))

(defun codex-attn--codex-vterm-buffer-p (buf)
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'vterm-mode)
              (string-prefix-p "*codex: " (buffer-name buf))))))

(defun codex-attn--vterm-buffers ()
  (let (out)
    (dolist (buf (buffer-list))
      (when (codex-attn--codex-vterm-buffer-p buf)
        (push buf out)))
    (nreverse out)))

(defun codex-attn--buffers-for-cwd (cwd)
  (let* ((target (codex-attn--normalize-dir cwd))
         (candidates nil))
    (when target
      (dolist (buf (codex-attn--vterm-buffers))
        (with-current-buffer buf
          (let ((bd (codex-attn--normalize-dir default-directory)))
            (when (and bd (string= target bd))
              (push buf candidates))))))
    (nreverse candidates)))

(defun codex-attn--find-vterm-by-cwd (cwd)
  (let ((candidates (codex-attn--buffers-for-cwd cwd)))
    (when (= (length candidates) 1)
      (car candidates))))

(defun codex-attn--queue-push (buf)
  (let ((now (float-time)))
  (setq codex-attn--pending-buffer-queue
        (cons (cons buf now)
              (seq-remove (lambda (entry) (eq (car-safe entry) buf))
                          codex-attn--pending-buffer-queue)))))

(defun codex-attn--queue-pop ()
  (let ((now (float-time))
        (found nil))
    (while (and codex-attn--pending-buffer-queue (not found))
      (let* ((entry (car codex-attn--pending-buffer-queue))
             (buf (car-safe entry))
             (ts (cdr-safe entry)))
        (setq codex-attn--pending-buffer-queue
              (cdr codex-attn--pending-buffer-queue))
        (when (and (numberp ts)
                   (<= (- now ts) codex-attn-queue-ttl)
                   (codex-attn--codex-vterm-buffer-p buf))
          (setq found buf))))
    found))

(defun codex-attn-register-current-buffer ()
  "Register current `*codex:*` vterm buffer for the next thread binding."
  (interactive)
  (if (codex-attn--codex-vterm-buffer-p (current-buffer))
      (progn
        (codex-attn--queue-push (current-buffer))
        (message "Codex: registered %s for next thread binding." (buffer-name)))
    (message "Codex: current buffer is not a *codex:* vterm.")))

(defun codex-attn--buffer-for-thread (thread-id)
  (let ((buf (gethash thread-id codex-attn--thread->buffer)))
    (if (buffer-live-p buf)
        buf
      (when buf
        (remhash thread-id codex-attn--thread->buffer))
      nil)))

(defun codex-attn--maybe-bind-thread (session)
  (let* ((thread-id (plist-get session :thread_id))
         (cwd (plist-get session :cwd)))
    (unless (codex-attn--buffer-for-thread thread-id)
      (let ((buf (or (codex-attn--find-vterm-by-cwd cwd)
                     (codex-attn--queue-pop))))
        (when (buffer-live-p buf)
          (puthash thread-id buf codex-attn--thread->buffer))))))

(defun codex-attn--auto-bind ()
  (dolist (session codex-attn--pending-sessions)
    (codex-attn--maybe-bind-thread session)))

(defun codex-attn--prune-thread-map ()
  (let ((active (make-hash-table :test 'equal)))
    (dolist (session codex-attn--pending-sessions)
      (let ((thread-id (plist-get session :thread_id)))
        (when (and thread-id (stringp thread-id))
          (puthash thread-id t active))))
    (maphash
     (lambda (thread-id _buf)
       (unless (gethash thread-id active)
         (remhash thread-id codex-attn--thread->buffer)))
     codex-attn--thread->buffer)))

(defun codex-attn--prune-buffer-queue ()
  (let ((now (float-time)))
    (setq codex-attn--pending-buffer-queue
          (seq-filter
           (lambda (entry)
             (let ((buf (car-safe entry))
                   (ts (cdr-safe entry)))
               (and (buffer-live-p buf)
                    (numberp ts)
                    (<= (- now ts) codex-attn-queue-ttl))))
           codex-attn--pending-buffer-queue))))

(defun codex-attn--read-state-dir ()
  (let ((dir (file-name-as-directory codex-attn-state-dir))
        (sessions nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json\\'"))
        (condition-case _err
            (let* ((json-object-type 'plist)
                   (json-key-type 'keyword)
                   (json-array-type 'list)
                   (json-false nil)
                   (data (json-read-file file)))
              (setq data (plist-put data :file file))
              (push data sessions))
          (error nil))))
    sessions))

(defun codex-attn--refresh ()
  (setq codex-attn--pending-sessions (codex-attn--read-state-dir))
  (codex-attn--prune-buffer-queue)
  (codex-attn--prune-thread-map)
  (codex-attn--auto-bind)
  (if codex-attn--pending-sessions
      (codex-attn--start-blink)
    (codex-attn--stop-blink))
  (force-mode-line-update t))

(defun codex-attn--watch-callback (_event)
  (codex-attn--refresh))

(defun codex-attn--start-watch ()
  (codex-attn--ensure-dir)
  (unless codex-attn--watch
    (condition-case err
        (setq codex-attn--watch
              (file-notify-add-watch
               codex-attn-state-dir
               '(change attribute-change)
               #'codex-attn--watch-callback))
      (file-notify-error
       (setq codex-attn--watch nil)
       (message "Codex: file watch unavailable (%s), using polling."
                (error-message-string err)))
      (error
       (setq codex-attn--watch nil)
       (message "Codex: failed to start file watch (%s), using polling."
                (error-message-string err)))))
  (when (and (not codex-attn--watch)
             (not codex-attn--poll-timer))
    (setq codex-attn--poll-timer
          (run-with-timer 0 codex-attn-poll-interval #'codex-attn--refresh)))
  (when (and codex-attn--watch codex-attn--poll-timer)
    (cancel-timer codex-attn--poll-timer)
    (setq codex-attn--poll-timer nil))
  (codex-attn--refresh))

(defun codex-attn--stop-watch ()
  (when codex-attn--watch
    (condition-case nil
        (file-notify-rm-watch codex-attn--watch)
      (error nil))
    (setq codex-attn--watch nil))
  (when codex-attn--poll-timer
    (cancel-timer codex-attn--poll-timer)
    (setq codex-attn--poll-timer nil)))

(defun codex-attn--blink-tick ()
  (setq codex-attn--blink-on (not codex-attn--blink-on))
  (force-mode-line-update t))

(defun codex-attn--start-blink ()
  (unless codex-attn--blink-timer
    (setq codex-attn--blink-on t)
    (setq codex-attn--blink-timer
          (run-with-timer 0 codex-attn-blink-interval #'codex-attn--blink-tick))))

(defun codex-attn--stop-blink ()
  (when codex-attn--blink-timer
    (cancel-timer codex-attn--blink-timer))
  (setq codex-attn--blink-timer nil)
  (setq codex-attn--blink-on nil))

(defun codex-attn--modeline ()
  (when codex-attn--pending-sessions
    (propertize (format " %s " codex-attn-modeline-text)
                'face (if codex-attn--blink-on
                          'codex-attn-modeline-alert-face
                        'codex-attn-modeline-idle-face)
                'mouse-face 'mode-line-highlight
                'help-echo "Codex pending. mouse-1: jump to most recent"
                'local-map codex-attn--modeline-map)))

(defun codex-attn--session-pending-since (session)
  (or (plist-get session :pending_since)
      (plist-get session :last_event_ts)
      0))

(defun codex-attn--session-last-event (session)
  (or (plist-get session :last_event_ts)
      (plist-get session :pending_since)
      0))

(defun codex-attn--sort-sessions (sessions keyfn predicate)
  (sort (copy-sequence sessions)
        (lambda (a b)
          (funcall predicate
                   (funcall keyfn a)
                   (funcall keyfn b)))))

(defun codex-attn--format-session (session)
  (let* ((thread (or (plist-get session :thread_id) ""))
         (cwd (or (plist-get session :cwd) ""))
         (msg (or (plist-get session :last_assistant_message) ""))
         (short-thread (if (> (length thread) 8) (substring thread 0 8) thread))
         (short-cwd (if (string-empty-p cwd) "" (abbreviate-file-name cwd)))
         (clean-msg (string-trim (replace-regexp-in-string "[\r\n]+" " " msg))))
    (when (> (length clean-msg) 60)
      (setq clean-msg (concat (substring clean-msg 0 57) "...")))
    (string-trim (format "%s | %s | %s" short-thread short-cwd clean-msg))))

(defun codex-attn-pending-sessions (&optional order)
  "Return pending sessions as plists.
ORDER can be 'fifo or 'recent."
  (codex-attn--refresh)
  (let ((sessions codex-attn--pending-sessions))
    (pcase order
      ('recent (codex-attn--sort-sessions sessions #'codex-attn--session-last-event #'>))
      ('fifo (codex-attn--sort-sessions sessions #'codex-attn--session-pending-since #'<))
      (_ sessions))))

(defun codex-attn-pending-candidates (&optional order)
  "Return (DISPLAY . SESSION) candidates for consult."
  (mapcar (lambda (s) (cons (codex-attn--format-session s) s))
          (codex-attn-pending-sessions order)))

(defun codex-attn-list-pending ()
  "List pending sessions and return them."
  (interactive)
  (let ((sessions (codex-attn-pending-sessions 'fifo)))
    (if sessions
        (message "%s" (mapconcat #'codex-attn--format-session sessions "\n"))
      (message "Codex: no pending sessions."))
    sessions))

(defun codex-attn--ack-session (session)
  (let ((file (plist-get session :file)))
    (when (and file (file-exists-p file))
      (delete-file file)))
  (codex-attn--refresh))

(defun codex-attn--jump-to-session (session)
  (let* ((thread-id (plist-get session :thread_id))
         (cwd (plist-get session :cwd))
         (buf (and thread-id (codex-attn--buffer-for-thread thread-id))))
    (unless (buffer-live-p buf)
      (let ((by-cwd (codex-attn--buffers-for-cwd cwd)))
        (setq buf
              (cond
               ((= (length by-cwd) 1) (car by-cwd))
               ((> (length by-cwd) 1)
                (let* ((choices (mapcar (lambda (b)
                                          (cons (buffer-name b) b))
                                        by-cwd))
                       (picked (cdr (assoc (completing-read
                                            "Codex buffer for pending session: "
                                            choices nil t)
                                           choices))))
                  picked))
               (t nil))))
      (when (and thread-id (buffer-live-p buf))
        (puthash thread-id buf codex-attn--thread->buffer)))
    (if (buffer-live-p buf)
        (progn
          (pop-to-buffer buf)
          (codex-attn--ack-session session))
      (message "Codex: no buffer mapped for thread %s." thread-id))))

(defun codex-attn-jump-most-recent ()
  "Jump to the most recent pending session."
  (interactive)
  (let ((sessions (codex-attn-pending-sessions 'recent)))
    (if sessions
        (codex-attn--jump-to-session (car sessions))
      (message "Codex: no pending sessions."))))

(defun codex-attn-jump-fifo ()
  "Jump to the first pending session (FIFO)."
  (interactive)
  (let ((sessions (codex-attn-pending-sessions 'fifo)))
    (if sessions
        (codex-attn--jump-to-session (car sessions))
      (message "Codex: no pending sessions."))))

(defun codex-attn-refresh ()
  "Force refresh of Codex pending sessions."
  (interactive)
  (codex-attn--refresh))

(defun codex-attn-status ()
  "Return and display current Codex attention internals."
  (interactive)
  (let (mappings)
    (maphash (lambda (thread buf)
               (when (buffer-live-p buf)
                 (push (cons thread (buffer-name buf)) mappings)))
             codex-attn--thread->buffer)
    (setq mappings (nreverse mappings))
    (let ((status (list
                   :watch-active (and codex-attn--watch t)
                   :poll-active (and codex-attn--poll-timer t)
                   :pending-count (length codex-attn--pending-sessions)
                   :queue-count (length codex-attn--pending-buffer-queue)
                   :mappings mappings)))
      (message "Codex status: watch=%s poll=%s pending=%d queue=%d mapped=%d"
               (if codex-attn--watch "on" "off")
               (if codex-attn--poll-timer "on" "off")
               (length codex-attn--pending-sessions)
               (length codex-attn--pending-buffer-queue)
               (length mappings))
      status)))

(define-minor-mode codex-attn-mode
  "Global mode for Codex attention indicator."
  :global t
  (if codex-attn-mode
      (progn
        (codex-attn--start-watch)
        (add-to-list 'global-mode-string codex-attn--modeline-entry t))
    (codex-attn--stop-watch)
    (codex-attn--stop-blink)
    (setq global-mode-string (delq codex-attn--modeline-entry global-mode-string))
    (force-mode-line-update t)))


(provide 'codex-attn)

;;; codex-attn.el ends here
