;;; codex-attn.el --- Codex/OpenCode attention indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; Modeline indicator and session jumping for pending Codex/OpenCode turns.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'filenotify)

(defgroup codex-attn nil
  "Codex/OpenCode attention indicator."
  :group 'tools)

(defcustom codex-attn-state-dir
  (expand-file-name "codex/threads" (or (getenv "XDG_CACHE_HOME") "~/.cache/"))
  "Directory where Codex notify writes per-thread state files."
  :type 'directory)

(defcustom codex-attn-opencode-state-dir
  (expand-file-name "opencode/threads" (or (getenv "XDG_CACHE_HOME") "~/.cache/"))
  "Directory where OpenCode notify writes per-session state files."
  :type 'directory)

(defcustom codex-attn-providers
  `((codex :state-dir ,codex-attn-state-dir :buffer-prefix "*codex: ")
    (opencode :state-dir ,codex-attn-opencode-state-dir :buffer-prefix "*opencode: "))
  "Provider definitions.

Each entry is:
  (PROVIDER :state-dir DIR :buffer-prefix PREFIX)"
  :type '(repeat sexp))

(defcustom codex-attn-blink-interval 0.6
  "Seconds between blink frames."
  :type 'number)

(defcustom codex-attn-poll-interval 2.0
  "Polling interval in seconds when filesystem watch is unavailable."
  :type 'number)

(defcustom codex-attn-queue-ttl 120
  "Seconds before queued provider buffer candidates expire."
  :type 'number)

(defcustom codex-attn-modeline-text "Cdx!"
  "Modeline text when an agent needs attention."
  :type 'string)

(defface codex-attn-modeline-idle-face
  '((t :inherit mode-line :weight bold))
  "Face for modeline indicator when pending but blink-off.")

(defface codex-attn-modeline-alert-face
  '((t :inherit mode-line :weight bold :foreground "white" :background "firebrick3"))
  "Face for modeline indicator when pending and blink-on.")

(defvar codex-attn--pending-sessions nil)
(defvar codex-attn--thread->buffer (make-hash-table :test 'equal))
(defvar codex-attn--pending-buffer-queue nil)
(defvar codex-attn--snoozed-until (make-hash-table :test 'equal))
(defvar codex-attn--watches nil)
(defvar codex-attn--poll-timer nil)
(defvar codex-attn--blink-timer nil)
(defvar codex-attn--blink-on nil)

(defvar codex-attn--modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'codex-attn-jump-most-recent)
    map))

(defconst codex-attn--modeline-entry '(:eval (codex-attn--modeline)))
(defvar codex-attn-mode)

(defun codex-attn--provider-symbol (provider)
  (cond
   ((symbolp provider) provider)
   ((and (stringp provider) (not (string-empty-p provider)))
    (intern (downcase provider)))
   (t nil)))

(defun codex-attn--provider-entry (provider)
  (assq (codex-attn--provider-symbol provider) codex-attn-providers))

(defun codex-attn--provider-prop (provider prop)
  (plist-get (cdr (codex-attn--provider-entry provider)) prop))

(defun codex-attn--provider-state-dir (provider)
  (let ((p (codex-attn--provider-symbol provider)))
    (or (codex-attn--provider-prop p :state-dir)
        (pcase p
          ('codex codex-attn-state-dir)
          ('opencode codex-attn-opencode-state-dir)
          (_ nil)))))

(defun codex-attn--provider-buffer-prefix (provider)
  (codex-attn--provider-prop (codex-attn--provider-symbol provider) :buffer-prefix))

(defun codex-attn--normalize-dir (dir)
  (when (and dir (stringp dir))
    (ignore-errors (file-name-as-directory (file-truename dir)))))

(defun codex-attn--state-dirs ()
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (entry codex-attn-providers)
      (let* ((provider (car entry))
             (dir (codex-attn--normalize-dir (codex-attn--provider-state-dir provider))))
        (when (and dir (not (gethash dir seen)))
          (puthash dir t seen)
          (push dir out))))
    (nreverse out)))

(defun codex-attn--ensure-dirs ()
  (dolist (dir (codex-attn--state-dirs))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun codex-attn--buffer-provider (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (derived-mode-p 'vterm-mode)
        (let ((name (buffer-name buf))
              found)
          (dolist (entry codex-attn-providers found)
            (let ((provider (car entry))
                  (prefix (plist-get (cdr entry) :buffer-prefix)))
              (when (and (stringp prefix) (string-prefix-p prefix name))
                (setq found provider)))))))))

(defun codex-attn--provider-vterm-buffer-p (buf provider)
  (and (buffer-live-p buf)
       (eq (codex-attn--buffer-provider buf) (codex-attn--provider-symbol provider))))

(defun codex-attn--codex-vterm-buffer-p (buf)
  "Compatibility helper for callers expecting codex-only predicate."
  (codex-attn--provider-vterm-buffer-p buf 'codex))

(defun codex-attn--vterm-buffers (&optional provider)
  (let* ((target (codex-attn--provider-symbol provider))
         out)
    (dolist (buf (buffer-list))
      (let ((buf-provider (codex-attn--buffer-provider buf)))
        (when (and buf-provider
                   (or (null target) (eq buf-provider target)))
          (push buf out))))
    (nreverse out)))

(defun codex-attn--buffers-for-cwd (provider cwd)
  (let* ((target-provider (codex-attn--provider-symbol provider))
         (target-cwd (codex-attn--normalize-dir cwd))
         candidates)
    (when (and target-provider target-cwd)
      (dolist (buf (codex-attn--vterm-buffers target-provider))
        (with-current-buffer buf
          (let ((buf-cwd (codex-attn--normalize-dir default-directory)))
            (when (and buf-cwd (string= target-cwd buf-cwd))
              (push buf candidates))))))
    (nreverse candidates)))

(defun codex-attn--find-vterm-by-cwd (provider cwd)
  (let ((candidates (codex-attn--buffers-for-cwd provider cwd)))
    (when (= (length candidates) 1)
      (car candidates))))

(defun codex-attn--queue-push (buf &optional provider)
  "Queue BUF as a candidate for the next thread binding.

PROVIDER defaults to what can be inferred from BUF."
  (let* ((buf-provider (or (codex-attn--provider-symbol provider)
                           (codex-attn--buffer-provider buf)))
         (now (float-time)))
    (when (and buf-provider (buffer-live-p buf))
      (setq codex-attn--pending-buffer-queue
            (cons (list :provider buf-provider :buffer buf :ts now)
                  (seq-remove
                   (lambda (entry)
                     (eq (plist-get entry :buffer) buf))
                   codex-attn--pending-buffer-queue))))))

(defun codex-attn-queue-buffer (&optional buffer provider)
  "Queue BUFFER for next session mapping.

BUFFER defaults to current buffer.
PROVIDER is optional and inferred from BUFFER when omitted."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (resolved-provider (or (codex-attn--provider-symbol provider)
                                (codex-attn--buffer-provider buf))))
    (if resolved-provider
        (progn
          (codex-attn--queue-push buf resolved-provider)
          (message "codex-attn: queued %s for provider %s."
                   (buffer-name buf) resolved-provider))
      (message "codex-attn: buffer %s is not a tracked provider terminal."
               (buffer-name buf)))))

(defun codex-attn--queue-pop (&optional provider)
  (let* ((target-provider (codex-attn--provider-symbol provider))
         (now (float-time))
         (found nil)
         kept)
    (dolist (entry codex-attn--pending-buffer-queue)
      (let* ((buf (plist-get entry :buffer))
             (buf-provider (codex-attn--provider-symbol (plist-get entry :provider)))
             (ts (plist-get entry :ts))
             (valid (and (buffer-live-p buf)
                         (numberp ts)
                         (<= (- now ts) codex-attn-queue-ttl)
                         buf-provider
                         (codex-attn--provider-vterm-buffer-p buf buf-provider))))
        (when valid
          (if (and (not found)
                   (or (null target-provider)
                       (eq buf-provider target-provider)))
              (setq found buf)
            (push entry kept)))))
    (setq codex-attn--pending-buffer-queue (nreverse kept))
    found))

(defun codex-attn-register-current-buffer ()
  "Register current provider buffer for the next thread binding."
  (interactive)
  (let* ((buf (current-buffer))
         (provider (codex-attn--buffer-provider buf)))
    (if provider
        (progn
          (codex-attn--queue-push buf provider)
          (message "codex-attn: registered %s for provider %s."
                   (buffer-name buf) provider))
      (message "codex-attn: current buffer is not a tracked provider vterm."))))

(defun codex-attn--thread-key (provider thread-id)
  (when (and provider thread-id)
    (format "%s::%s" (symbol-name (codex-attn--provider-symbol provider)) thread-id)))

(defun codex-attn--buffer-for-thread (provider thread-id)
  (let* ((key (codex-attn--thread-key provider thread-id))
         (buf (and key (gethash key codex-attn--thread->buffer))))
    (if (buffer-live-p buf)
        buf
      (when key
        (remhash key codex-attn--thread->buffer))
      nil)))

(defun codex-attn--put-thread-buffer (provider thread-id buf)
  (let ((key (codex-attn--thread-key provider thread-id)))
    (when (and key (buffer-live-p buf))
      (puthash key buf codex-attn--thread->buffer))))

(defun codex-attn--current-attn-buffer ()
  (let ((buf (window-buffer (selected-window))))
    (when (codex-attn--buffer-provider buf)
      buf)))

(defun codex-attn--current-codex-buffer ()
  "Compatibility helper for callers expecting codex-only current buffer."
  (let ((buf (codex-attn--current-attn-buffer)))
    (when (eq (codex-attn--buffer-provider buf) 'codex)
      buf)))

(defun codex-attn--session-provider (session)
  (or (codex-attn--provider-symbol (plist-get session :provider))
      'codex))

(defun codex-attn--session-thread-id (session)
  (let ((thread-id (plist-get session :thread_id)))
    (when thread-id
      (format "%s" thread-id))))

(defun codex-attn--session-cwd (session)
  (plist-get session :cwd))

(defun codex-attn--session-key (session)
  (or (plist-get session :file)
      (codex-attn--thread-key
       (codex-attn--session-provider session)
       (codex-attn--session-thread-id session))))

(defun codex-attn--session-has-associated-buffer-p (session)
  (let* ((provider (codex-attn--session-provider session))
         (thread-id (codex-attn--session-thread-id session))
         (cwd (codex-attn--session-cwd session))
         (mapped (and thread-id (codex-attn--buffer-for-thread provider thread-id))))
    (or (buffer-live-p mapped)
        (and (stringp cwd)
             (consp (codex-attn--buffers-for-cwd provider cwd))))))

(defun codex-attn--actionable-sessions (sessions)
  (seq-filter #'codex-attn--session-has-associated-buffer-p sessions))

(defun codex-attn--session-targets-buffer-p (session buf)
  (let* ((provider (codex-attn--session-provider session))
         (thread-id (codex-attn--session-thread-id session))
         (cwd (codex-attn--session-cwd session))
         (mapped (and thread-id (codex-attn--buffer-for-thread provider thread-id))))
    (cond
     ((not (eq (codex-attn--buffer-provider buf) provider)) nil)
     ((eq mapped buf) t)
     ((and cwd (stringp cwd))
      (let ((candidates (codex-attn--buffers-for-cwd provider cwd)))
        (when (and (= (length candidates) 1)
                   (eq (car candidates) buf))
          (when (and thread-id (not mapped))
            (codex-attn--put-thread-buffer provider thread-id buf))
          t)))
     (t nil))))

(defun codex-attn--visible-pending-sessions ()
  (let* ((buf (codex-attn--current-attn-buffer))
         (sessions (codex-attn--actionable-sessions codex-attn--pending-sessions)))
    (if (not (buffer-live-p buf))
        sessions
      (seq-remove
       (lambda (session)
         (codex-attn--session-targets-buffer-p session buf))
       sessions))))

(defun codex-attn--ack-visible-sessions ()
  (let ((buf (codex-attn--current-attn-buffer))
        (changed nil))
    (when (buffer-live-p buf)
      (dolist (session codex-attn--pending-sessions)
        (when (codex-attn--session-targets-buffer-p session buf)
          (let ((file (plist-get session :file)))
            (when (and file (file-exists-p file))
              (delete-file file)
              (setq changed t))))))
    changed))

(defun codex-attn--maybe-bind-thread (session)
  (let* ((provider (codex-attn--session-provider session))
         (thread-id (codex-attn--session-thread-id session))
         (cwd (codex-attn--session-cwd session)))
    (when (and provider thread-id (not (codex-attn--buffer-for-thread provider thread-id)))
      (let ((buf (or (codex-attn--find-vterm-by-cwd provider cwd)
                     (codex-attn--queue-pop provider))))
        (when (buffer-live-p buf)
          (codex-attn--put-thread-buffer provider thread-id buf))))))

(defun codex-attn--auto-bind ()
  (dolist (session codex-attn--pending-sessions)
    (codex-attn--maybe-bind-thread session)))

(defun codex-attn--prune-thread-map ()
  (let ((active (make-hash-table :test 'equal)))
    (dolist (session codex-attn--pending-sessions)
      (let* ((provider (codex-attn--session-provider session))
             (thread-id (codex-attn--session-thread-id session))
             (key (codex-attn--thread-key provider thread-id)))
        (when key
          (puthash key t active))))
    (maphash
     (lambda (key _buf)
       (unless (gethash key active)
         (remhash key codex-attn--thread->buffer)))
     codex-attn--thread->buffer)))

(defun codex-attn--prune-buffer-queue ()
  (let ((now (float-time)))
    (setq codex-attn--pending-buffer-queue
          (seq-filter
           (lambda (entry)
             (let* ((buf (plist-get entry :buffer))
                    (provider (codex-attn--provider-symbol (plist-get entry :provider)))
                    (ts (plist-get entry :ts)))
               (and (buffer-live-p buf)
                    provider
                    (codex-attn--provider-vterm-buffer-p buf provider)
                    (numberp ts)
                    (<= (- now ts) codex-attn-queue-ttl))))
           codex-attn--pending-buffer-queue))))

(defun codex-attn--plist-first (plist keys)
  (let ((rest keys)
        (value nil)
        (found nil))
    (while rest
      (let ((candidate (plist-get plist (car rest))))
        (when (not (null candidate))
          (setq value candidate)
          (setq found t)
          (setq rest nil)))
      (setq rest (cdr rest)))
    (when found value)))

(defun codex-attn--as-number (value)
  (cond
   ((numberp value) value)
   ((and (stringp value) (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" value))
    (string-to-number value))
   (t nil)))

(defun codex-attn--normalize-session-data (provider data file)
  (let ((thread-id (codex-attn--plist-first
                    data
                    '(:thread_id :threadId :thread-id :session_id :sessionId :sessionID :id)))
        (turn-id (codex-attn--plist-first data '(:turn_id :turnId :turn-id)))
        (cwd (codex-attn--plist-first
              data
              '(:cwd :working_directory :working-directory :workingDirectory :path)))
        (last-msg (codex-attn--plist-first
                   data
                   '(:last_assistant_message
                     :lastAssistantMessage
                     :last-assistant-message
                     :assistant_message
                     :assistantMessage
                     :message)))
        (pending-since (codex-attn--as-number
                        (codex-attn--plist-first data '(:pending_since :pendingSince))))
        (last-event-ts (codex-attn--as-number
                        (codex-attn--plist-first data '(:last_event_ts :lastEventTs :timestamp))))
        (event-type (codex-attn--plist-first data '(:type :event_type :eventType))))
    (list :provider (codex-attn--provider-symbol provider)
          :thread_id (and thread-id (format "%s" thread-id))
          :turn_id turn-id
          :cwd cwd
          :last_assistant_message (and last-msg (format "%s" last-msg))
          :pending_since pending-since
          :last_event_ts last-event-ts
          :type event-type
          :file file)))

(defun codex-attn--read-provider-state-dir (provider)
  (let* ((state-dir (codex-attn--provider-state-dir provider))
         (dir (and state-dir (file-name-as-directory state-dir)))
        sessions)
    (when (and dir (file-directory-p dir))
      (dolist (file (directory-files dir t "\\.json\\'"))
        (condition-case _err
            (let* ((json-object-type 'plist)
                   (json-key-type 'keyword)
                   (json-array-type 'list)
                   (json-false nil)
                   (raw (json-read-file file))
                   (session (codex-attn--normalize-session-data provider raw file)))
              (push session sessions))
          (error nil))))
    sessions))

(defun codex-attn--read-state-dir ()
  "Compatibility helper; now reads all configured provider state dirs."
  (let (sessions)
    (dolist (entry codex-attn-providers)
      (setq sessions (nconc (codex-attn--read-provider-state-dir (car entry))
                            sessions)))
    sessions))

(defun codex-attn--filter-snoozed-sessions (sessions)
  (let ((now (float-time))
        (active (make-hash-table :test 'equal))
        visible)
    (dolist (session sessions)
      (let* ((key (codex-attn--session-key session))
             (until (and key (gethash key codex-attn--snoozed-until))))
        (when key
          (puthash key t active))
        (if (and (numberp until) (> until now))
            nil
          (when key
            (remhash key codex-attn--snoozed-until))
          (push session visible))))
    (maphash
     (lambda (key _until)
       (unless (gethash key active)
         (remhash key codex-attn--snoozed-until)))
     codex-attn--snoozed-until)
    (nreverse visible)))

(defun codex-attn--refresh ()
  (setq codex-attn--pending-sessions
        (codex-attn--filter-snoozed-sessions (codex-attn--read-state-dir)))
  (codex-attn--prune-buffer-queue)
  (codex-attn--prune-thread-map)
  (codex-attn--auto-bind)
  ;; If the user is already in the target provider buffer, auto-ack.
  (when (codex-attn--ack-visible-sessions)
    (setq codex-attn--pending-sessions
          (codex-attn--filter-snoozed-sessions (codex-attn--read-state-dir)))
    (codex-attn--prune-thread-map)
    (codex-attn--auto-bind))
  (if (codex-attn--visible-pending-sessions)
      (codex-attn--start-blink)
    (codex-attn--stop-blink))
  (force-mode-line-update t))

(defun codex-attn--watch-callback (_event)
  (codex-attn--refresh))

(defun codex-attn--start-watch ()
  (codex-attn--ensure-dirs)
  (unless codex-attn--watches
    (let ((need-poll nil))
      (dolist (dir (codex-attn--state-dirs))
        (condition-case err
            (push (file-notify-add-watch
                   dir
                   '(change attribute-change)
                   #'codex-attn--watch-callback)
                  codex-attn--watches)
          (file-notify-error
           (setq need-poll t)
           (message "codex-attn: file watch unavailable for %s (%s), using polling fallback."
                    dir (error-message-string err)))
          (error
           (setq need-poll t)
           (message "codex-attn: failed to watch %s (%s), using polling fallback."
                    dir (error-message-string err)))))
      (setq codex-attn--watches (nreverse codex-attn--watches))
      (when (or need-poll (null codex-attn--watches))
        (unless codex-attn--poll-timer
          (setq codex-attn--poll-timer
                (run-with-timer 0 codex-attn-poll-interval #'codex-attn--refresh))))
      (when (and (not need-poll) codex-attn--watches codex-attn--poll-timer)
        (cancel-timer codex-attn--poll-timer)
        (setq codex-attn--poll-timer nil))))
  (codex-attn--refresh))

(defun codex-attn--stop-watch ()
  (dolist (watch codex-attn--watches)
    (condition-case nil
        (file-notify-rm-watch watch)
      (error nil)))
  (setq codex-attn--watches nil)
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
  (when (codex-attn--visible-pending-sessions)
    (propertize (format " %s " codex-attn-modeline-text)
                'face (if codex-attn--blink-on
                          'codex-attn-modeline-alert-face
                        'codex-attn-modeline-idle-face)
                'mouse-face 'mode-line-highlight
                'help-echo "Agent pending. mouse-1: jump to most recent"
                'local-map codex-attn--modeline-map)))

(defun codex-attn--on-buffer-visibility-change (&rest _)
  (when codex-attn-mode
    (when (codex-attn--ack-visible-sessions)
      (codex-attn--refresh))
    (force-mode-line-update t)))

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
  (let* ((provider (symbol-name (codex-attn--session-provider session)))
         (thread (or (codex-attn--session-thread-id session) ""))
         (cwd (or (plist-get session :cwd) ""))
         (msg (or (plist-get session :last_assistant_message) ""))
         (short-thread (if (> (length thread) 8) (substring thread 0 8) thread))
         (short-cwd (if (string-empty-p cwd) "" (abbreviate-file-name cwd)))
         (clean-msg (string-trim (replace-regexp-in-string "[\r\n]+" " " msg))))
    (when (> (length clean-msg) 60)
      (setq clean-msg (concat (substring clean-msg 0 57) "...")))
    (string-trim (format "[%s] %s | %s | %s"
                         provider short-thread short-cwd clean-msg))))

(defun codex-attn-pending-sessions (&optional order)
  "Return pending sessions as plists.
ORDER can be `fifo` or `recent`."
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
      (message "codex-attn: no pending sessions."))
    sessions))

(defun codex-attn--ack-session (session)
  (let ((file (plist-get session :file)))
    (when (and file (file-exists-p file))
      (delete-file file)))
  (codex-attn--refresh))

(defun codex-attn-clear-pending ()
  "Delete all pending session state files."
  (interactive)
  (let ((count 0))
    (dolist (session (codex-attn--read-state-dir))
      (let ((file (plist-get session :file)))
        (when (and file (file-exists-p file))
          (delete-file file)
          (setq count (1+ count)))))
    (clrhash codex-attn--snoozed-until)
    (codex-attn--refresh)
    (message "codex-attn: cleared %d pending session%s."
             count
             (if (= count 1) "" "s"))))

(defun codex-attn-snooze-pending (minutes)
  "Snooze all currently pending sessions for MINUTES."
  (interactive (list (read-number "codex-attn snooze minutes: " 10)))
  (let* ((mins (max 0 minutes))
         (until (+ (float-time) (* mins 60)))
         (sessions (codex-attn--read-state-dir))
         (count 0))
    (if (= mins 0)
        (progn
          (clrhash codex-attn--snoozed-until)
          (codex-attn--refresh)
          (message "codex-attn: cleared all snoozes."))
      (dolist (session sessions)
        (let ((key (codex-attn--session-key session)))
          (when key
            (puthash key until codex-attn--snoozed-until)
            (setq count (1+ count)))))
      (codex-attn--refresh)
      (message "codex-attn: snoozed %d pending session%s for %d minute%s."
               count
               (if (= count 1) "" "s")
               mins
               (if (= mins 1) "" "s")))))

(defun codex-attn--jump-to-session (session)
  (let* ((provider (codex-attn--session-provider session))
         (thread-id (codex-attn--session-thread-id session))
         (cwd (codex-attn--session-cwd session))
         (buf (and thread-id (codex-attn--buffer-for-thread provider thread-id))))
    (unless (buffer-live-p buf)
      (let ((by-cwd (codex-attn--buffers-for-cwd provider cwd)))
        (setq buf
              (cond
               ((= (length by-cwd) 1) (car by-cwd))
               ((> (length by-cwd) 1)
                (let* ((choices (mapcar (lambda (b)
                                          (cons (buffer-name b) b))
                                        by-cwd))
                       (picked (cdr (assoc (completing-read
                                            (format "Buffer for pending %s session: " provider)
                                            choices nil t)
                                           choices))))
                  picked))
               (t nil))))
      (when (and thread-id (buffer-live-p buf))
        (codex-attn--put-thread-buffer provider thread-id buf)))
    (if (buffer-live-p buf)
        (progn
          (pop-to-buffer buf)
          (codex-attn--ack-session session))
      (message "codex-attn: no buffer mapped for %s thread %s."
               provider (or thread-id "unknown")))))

(defun codex-attn-jump-most-recent ()
  "Jump to the most recent pending session."
  (interactive)
  (let ((sessions (codex-attn-pending-sessions 'recent)))
    (if sessions
        (codex-attn--jump-to-session (car sessions))
      (message "codex-attn: no pending sessions."))))

(defun codex-attn-jump-fifo ()
  "Jump to the first pending session (FIFO)."
  (interactive)
  (let ((sessions (codex-attn-pending-sessions 'fifo)))
    (if sessions
        (codex-attn--jump-to-session (car sessions))
      (message "codex-attn: no pending sessions."))))

(defun codex-attn-refresh ()
  "Force refresh of pending sessions."
  (interactive)
  (codex-attn--refresh))

(defun codex-attn-status ()
  "Return and display current attention internals."
  (interactive)
  (let (mappings)
    (maphash (lambda (thread buf)
               (when (buffer-live-p buf)
                 (push (cons thread (buffer-name buf)) mappings)))
             codex-attn--thread->buffer)
    (setq mappings (nreverse mappings))
    (let ((status (list
                   :watch-count (length codex-attn--watches)
                   :poll-active (and codex-attn--poll-timer t)
                   :pending-count (length codex-attn--pending-sessions)
                   :snoozed-count (hash-table-count codex-attn--snoozed-until)
                   :queue-count (length codex-attn--pending-buffer-queue)
                   :mappings mappings)))
      (message "codex-attn status: watches=%d poll=%s pending=%d snoozed=%d queue=%d mapped=%d"
               (length codex-attn--watches)
               (if codex-attn--poll-timer "on" "off")
               (length codex-attn--pending-sessions)
               (hash-table-count codex-attn--snoozed-until)
               (length codex-attn--pending-buffer-queue)
               (length mappings))
      status)))

(define-minor-mode codex-attn-mode
  "Global mode for Codex/OpenCode attention indicator."
  :global t
  (if codex-attn-mode
      (progn
        (codex-attn--start-watch)
        (add-hook 'window-selection-change-functions #'codex-attn--on-buffer-visibility-change)
        (add-hook 'window-buffer-change-functions #'codex-attn--on-buffer-visibility-change)
        (add-to-list 'global-mode-string codex-attn--modeline-entry t))
    (remove-hook 'window-selection-change-functions #'codex-attn--on-buffer-visibility-change)
    (remove-hook 'window-buffer-change-functions #'codex-attn--on-buffer-visibility-change)
    (codex-attn--stop-watch)
    (codex-attn--stop-blink)
    (setq global-mode-string (delq codex-attn--modeline-entry global-mode-string))
    (force-mode-line-update t)))

(provide 'codex-attn)

;;; codex-attn.el ends here
