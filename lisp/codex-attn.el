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

(defcustom codex-attn-refresh-delay 0.15
  "Seconds used to coalesce bursts of filesystem notifications."
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
(defvar codex-attn--actionable-session-list nil)
(defvar codex-attn--terminal-id->buffer (make-hash-table :test 'equal))
(defvar codex-attn--thread-id->buffer (make-hash-table :test 'equal))
(defvar codex-attn--sessions-by-buffer (make-hash-table :test 'eq))
(defvar codex-attn--state-cache (make-hash-table :test 'equal))
(defvar codex-attn--snoozed-until (make-hash-table :test 'equal))
(defvar codex-attn--watches nil)
(defvar codex-attn--poll-timer nil)
(defvar codex-attn--refresh-timer nil)
(defvar codex-attn--blink-timer nil)
(defvar codex-attn--blink-on nil)
(defvar codex-attn--visible-pending nil)

(defvar codex-attn-state-change-hook nil
  "Hook run after Codex attention or terminal-buffer state changes.

Functions on this hook receive no arguments.  Consumers should use the public
query functions such as `codex-attn-buffers' and
`codex-attn-buffer-needs-attention-p' instead of modifying notifier state.")

(defun codex-attn--new-id (kind)
  (substring
   (secure-hash 'sha256
                (format "%s:%s:%s:%s:%s"
                        kind (emacs-pid) (float-time) (random) (user-uid)))
   0 24))

;; `defvar' deliberately preserves this across source reloads.  A new Emacs
;; process gets a new value, while months-long sessions retain one identity.
(defvar codex-attn--emacs-instance-id (codex-attn--new-id "emacs"))
(defvar-local codex-attn-terminal-id nil)
(defvar-local codex-attn-thread-id nil)
(defvar-local codex-attn--terminal-cleaned-up nil)

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

(defun codex-attn--environment-with-identity (environment terminal-id)
  (let ((prefixes '("CODEX_ATTN_EMACS_INSTANCE_ID="
                    "CODEX_ATTN_TERMINAL_ID=")))
    (append
     (list (concat (car prefixes) codex-attn--emacs-instance-id)
           (concat (cadr prefixes) terminal-id))
     (seq-remove
      (lambda (entry)
        (and (stringp entry)
             (seq-some (lambda (prefix) (string-prefix-p prefix entry)) prefixes)))
      environment))))

(defun codex-attn--ghostel-setup ()
  "Give the current Ghostel buffer a stable notifier identity."
  (unless codex-attn-terminal-id
    (setq-local codex-attn-terminal-id (codex-attn--new-id "terminal")))
  (setq-local ghostel-environment
              (codex-attn--environment-with-identity
               (and (boundp 'ghostel-environment) ghostel-environment)
               codex-attn-terminal-id))
  (setq-local codex-attn--terminal-cleaned-up nil)
  (puthash codex-attn-terminal-id (current-buffer)
           codex-attn--terminal-id->buffer)
  (add-hook 'kill-buffer-hook #'codex-attn--terminal-buffer-killed nil t)
  (run-hooks 'codex-attn-state-change-hook))

;; Ghostel runs this mode hook before starting the child process, so both IDs
;; become part of that process' environment without changing Codex itself.
(add-hook 'ghostel-mode-hook #'codex-attn--ghostel-setup)

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
      ;; Previous terminal backend: (derived-mode-p 'vterm-mode)
      (when (derived-mode-p 'ghostel-mode)
        (let ((name (buffer-name buf))
              found
              found-prefix-len)
          (dolist (entry codex-attn-providers found)
            (let ((provider (car entry))
                  (prefix (plist-get (cdr entry) :buffer-prefix)))
              (when (and (stringp prefix) (string-prefix-p prefix name))
                (let ((prefix-len (length prefix)))
                  ;; Prefer the longest matching prefix so subset prefixes
                  ;; do not steal buffers from more specific providers.
                  (when (or (null found-prefix-len)
                            (> prefix-len found-prefix-len))
                    (setq found provider)
                    (setq found-prefix-len prefix-len)))))))))))

(defun codex-attn--provider-terminal-buffer-p (buf provider)
  (and (buffer-live-p buf)
       (eq (codex-attn--buffer-provider buf) (codex-attn--provider-symbol provider))))

;; Previous VTerm-specific compatibility helpers:
;; (defalias 'codex-attn--provider-vterm-buffer-p
;;   #'codex-attn--provider-terminal-buffer-p)
;; (defun codex-attn--codex-vterm-buffer-p (buf)
;;   "Compatibility helper for callers expecting codex-only predicate."
;;   (codex-attn--provider-vterm-buffer-p buf 'codex))

(defun codex-attn--terminal-buffers (&optional provider)
  (let* ((target (codex-attn--provider-symbol provider))
         out)
    (dolist (buf (buffer-list))
      (let ((buf-provider (codex-attn--buffer-provider buf)))
        (when (and buf-provider
                   (or (null target) (eq buf-provider target)))
          (push buf out))))
    (nreverse out)))

(defun codex-attn-buffers (&optional provider)
  "Return live notifier buffers, optionally restricted to PROVIDER.

The returned buffer objects retain their identity when their names change."
  (codex-attn--terminal-buffers provider))

(defun codex-attn-buffer-provider (buffer)
  "Return the notifier provider associated with BUFFER, or nil."
  (codex-attn--buffer-provider buffer))

(defun codex-attn-provider-buffer-prefix (provider)
  "Return the configured buffer-name prefix for PROVIDER."
  (codex-attn--provider-buffer-prefix provider))

(defun codex-attn-buffer-needs-attention-p (buffer)
  "Return non-nil when BUFFER has an actionable pending session."
  (and (buffer-live-p buffer)
       (consp (gethash buffer codex-attn--sessions-by-buffer))))

;; (defalias 'codex-attn--vterm-buffers #'codex-attn--terminal-buffers)

(defun codex-attn--rebuild-buffer-index ()
  (clrhash codex-attn--terminal-id->buffer)
  (clrhash codex-attn--thread-id->buffer)
  (dolist (buf (codex-attn--terminal-buffers))
    (with-current-buffer buf
      (unless codex-attn-terminal-id
        (codex-attn--ghostel-setup))
      (puthash codex-attn-terminal-id buf codex-attn--terminal-id->buffer)
      (when codex-attn-thread-id
        (puthash codex-attn-thread-id buf codex-attn--thread-id->buffer)))))

(defun codex-attn--buffer-for-terminal-id (terminal-id)
  (let ((buf (and terminal-id
                  (gethash terminal-id codex-attn--terminal-id->buffer))))
    (if (buffer-live-p buf)
        buf
      (when terminal-id
        (remhash terminal-id codex-attn--terminal-id->buffer))
      nil)))

(defun codex-attn--buffer-for-thread-id (thread-id)
  (let ((buf (and thread-id
                  (gethash thread-id codex-attn--thread-id->buffer))))
    (if (buffer-live-p buf)
        buf
      (when thread-id
        (remhash thread-id codex-attn--thread-id->buffer))
      nil)))

(defun codex-attn--record-thread-binding (buffer thread-id)
  (when (and (buffer-live-p buffer)
             (stringp thread-id)
             (not (string-empty-p thread-id)))
    (with-current-buffer buffer
      (when (and codex-attn-thread-id
                 (eq (gethash codex-attn-thread-id
                              codex-attn--thread-id->buffer)
                     buffer))
        (remhash codex-attn-thread-id codex-attn--thread-id->buffer))
      (setq-local codex-attn-thread-id thread-id))
    (puthash thread-id buffer codex-attn--thread-id->buffer)
    buffer))

;;;###autoload
(defun codex-attn-bind-buffer-thread (buffer thread-id)
  "Bind Codex terminal BUFFER to app-server THREAD-ID."
  (unless (codex-attn--provider-terminal-buffer-p buffer 'codex)
    (error "Not a live Codex terminal buffer: %s" buffer))
  (codex-attn--record-thread-binding buffer thread-id)
  (if (and (boundp 'codex-attn-mode) codex-attn-mode)
      (codex-attn--schedule-refresh)
    (run-hooks 'codex-attn-state-change-hook))
  thread-id)

(defun codex-attn--current-attn-buffer ()
  (let ((buf (window-buffer (selected-window))))
    (when (codex-attn--buffer-provider buf)
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

(defun codex-attn--session-terminal-id (session)
  (plist-get session :terminal_id))

(defun codex-attn--session-emacs-instance-id (session)
  (plist-get session :emacs_instance_id))

(defun codex-attn--session-has-identity-p (session)
  (and (codex-attn--session-emacs-instance-id session)
       (codex-attn--session-terminal-id session)))

(defun codex-attn--session-owned-p (session)
  (and (codex-attn--session-has-identity-p session)
       (equal (codex-attn--session-emacs-instance-id session)
              codex-attn--emacs-instance-id)))

(defun codex-attn--session-relevant-p (session)
  (or (codex-attn--buffer-for-thread-id
       (codex-attn--session-thread-id session))
      (codex-attn--session-owned-p session)))

(defun codex-attn--session-key (session)
  (plist-get session :file))

(defun codex-attn--buffer-for-session (session)
  (or (codex-attn--buffer-for-thread-id
       (codex-attn--session-thread-id session))
      (when (codex-attn--session-owned-p session)
        (let ((buffer
               (codex-attn--buffer-for-terminal-id
                (codex-attn--session-terminal-id session))))
          ;; Standalone Codex still identifies itself through the terminal
          ;; environment.  Learn its canonical thread id on the first event.
          (when (and buffer (codex-attn--session-thread-id session))
            (codex-attn--record-thread-binding
             buffer (codex-attn--session-thread-id session)))
          buffer))))

(defun codex-attn--visible-pending-sessions ()
  codex-attn--actionable-session-list)

(defun codex-attn--forget-session (session &optional delete-file-p)
  (let ((file (plist-get session :file))
        (key (codex-attn--session-key session)))
    (when file
      (remhash file codex-attn--state-cache)
      (when (and delete-file-p (file-exists-p file))
        (condition-case nil (delete-file file) (file-error nil))))
    (when key
      (remhash key codex-attn--snoozed-until))
    (setq codex-attn--pending-sessions
          (delq session codex-attn--pending-sessions))))

(defun codex-attn--ack-visible-sessions ()
  (let* ((buf (codex-attn--current-attn-buffer))
         (sessions (and (buffer-live-p buf)
                        (copy-sequence
                         (gethash buf codex-attn--sessions-by-buffer)))))
    (dolist (session sessions)
      (codex-attn--forget-session session t))
    (when sessions
      (codex-attn--rebuild-actionable-index))
    (consp sessions)))

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
        (emacs-instance-id
         (codex-attn--plist-first data '(:emacs_instance_id :emacsInstanceId)))
        (terminal-id
         (codex-attn--plist-first data '(:terminal_id :terminalId)))
        (event-type (codex-attn--plist-first data '(:type :event_type :eventType))))
    (list :provider (codex-attn--provider-symbol provider)
          :thread_id (and thread-id (format "%s" thread-id))
          :turn_id turn-id
          :cwd cwd
          :emacs_instance_id (and emacs-instance-id (format "%s" emacs-instance-id))
          :terminal_id (and terminal-id (format "%s" terminal-id))
          :last_assistant_message (and last-msg (format "%s" last-msg))
          :pending_since pending-since
          :last_event_ts last-event-ts
          :type event-type
          :file file)))

(defun codex-attn--file-signature (file)
  (let ((attrs (file-attributes file 'string)))
    (and attrs
         (list (file-attribute-size attrs)
               (file-attribute-modification-time attrs)
               (file-attribute-inode-number attrs)))))

(defun codex-attn--read-session-file (provider file)
  (condition-case nil
      (let* ((json-object-type 'plist)
             (json-key-type 'keyword)
             (json-array-type 'list)
             (json-false nil)
             (raw (json-read-file file)))
        (codex-attn--normalize-session-data provider raw file))
    (error nil)))

(defun codex-attn--sync-state-cache ()
  "Reconcile state filenames, parsing only new or changed JSON files."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (entry codex-attn-providers)
      (let* ((provider (car entry))
             (dir (codex-attn--provider-state-dir provider)))
        (when (and dir (file-directory-p dir))
          (dolist (file (directory-files dir t "\\.json\\'"))
            (puthash file t seen)
            (let* ((signature (codex-attn--file-signature file))
                   (cached (gethash file codex-attn--state-cache)))
              (unless (equal signature (plist-get cached :signature))
                (let ((session (codex-attn--read-session-file provider file)))
                  (puthash file (list :signature signature :session session)
                           codex-attn--state-cache))))))))
    (maphash
     (lambda (file _entry)
       (unless (gethash file seen)
         (remhash file codex-attn--state-cache)))
     codex-attn--state-cache)))

(defun codex-attn--cached-sessions ()
  (let (sessions)
    (maphash
     (lambda (_file entry)
       (let ((session (plist-get entry :session)))
         (when (and session (codex-attn--session-relevant-p session))
           (push session sessions))))
     codex-attn--state-cache)
    sessions))

(defun codex-attn--remove-orphaned-owned-sessions (sessions)
  (dolist (session (copy-sequence sessions))
    (when (and (codex-attn--session-terminal-id session)
               (equal (codex-attn--session-emacs-instance-id session)
                      codex-attn--emacs-instance-id)
               (not (codex-attn--buffer-for-terminal-id
                     (codex-attn--session-terminal-id session))))
      (codex-attn--forget-session session t))))

(defun codex-attn--read-state-dir ()
  "Read all configured provider state directories through the cache."
  (codex-attn--sync-state-cache)
  (codex-attn--cached-sessions))

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

(defun codex-attn--rebuild-actionable-index ()
  (clrhash codex-attn--sessions-by-buffer)
  (setq codex-attn--actionable-session-list nil)
  (dolist (session codex-attn--pending-sessions)
    (let ((buf (codex-attn--buffer-for-session session)))
      (when (buffer-live-p buf)
        (push session codex-attn--actionable-session-list)
        (puthash buf (cons session (gethash buf codex-attn--sessions-by-buffer))
                 codex-attn--sessions-by-buffer))))
  (setq codex-attn--actionable-session-list
        (nreverse codex-attn--actionable-session-list)))

(defun codex-attn--update-indicator ()
  (setq codex-attn--visible-pending
        (consp codex-attn--actionable-session-list))
  (if codex-attn--visible-pending
      (codex-attn--start-blink)
    (codex-attn--stop-blink))
  (force-mode-line-update)
  (run-hooks 'codex-attn-state-change-hook))

(defun codex-attn--terminal-buffer-killed ()
  "Remove state owned by the current terminal buffer, idempotently."
  (unless codex-attn--terminal-cleaned-up
    (setq codex-attn--terminal-cleaned-up t)
    (let ((terminal-id codex-attn-terminal-id)
          owned)
      ;; Include snoozed sessions: they remain in the cache even though they
      ;; are intentionally absent from `codex-attn--pending-sessions'.
      (dolist (session (codex-attn--cached-sessions))
        (when (equal terminal-id (codex-attn--session-terminal-id session))
          (push session owned)))
      (dolist (session owned)
        (codex-attn--forget-session session t))
      (when terminal-id
        (remhash terminal-id codex-attn--terminal-id->buffer))
      (when (and codex-attn-thread-id
                 (eq (gethash codex-attn-thread-id
                              codex-attn--thread-id->buffer)
                     (current-buffer)))
        (remhash codex-attn-thread-id codex-attn--thread-id->buffer))
      (codex-attn--rebuild-actionable-index)
      (codex-attn--update-indicator))))

(defun codex-attn--refresh ()
  (codex-attn--rebuild-buffer-index)
  (codex-attn--sync-state-cache)
  (let ((sessions (codex-attn--cached-sessions)))
    (setq codex-attn--pending-sessions sessions)
    (codex-attn--remove-orphaned-owned-sessions sessions))
  (setq codex-attn--pending-sessions
        (codex-attn--filter-snoozed-sessions
         (codex-attn--cached-sessions)))
  (codex-attn--rebuild-actionable-index)
  ;; If the user is already in the target provider buffer, auto-ack.
  (codex-attn--ack-visible-sessions)
  (codex-attn--update-indicator))

(defun codex-attn--run-scheduled-refresh ()
  (setq codex-attn--refresh-timer nil)
  (when codex-attn-mode
    (codex-attn--refresh)))

(defun codex-attn--schedule-refresh (&rest _)
  (unless codex-attn--refresh-timer
    (setq codex-attn--refresh-timer
          (run-at-time codex-attn-refresh-delay nil
                       #'codex-attn--run-scheduled-refresh))))

(defun codex-attn--watch-callback (event)
  (when (or (memq (cadr event) '(stopped renamed created deleted changed))
            (seq-some (lambda (part)
                        (and (stringp part) (string-suffix-p ".json" part)))
                      event))
    (codex-attn--schedule-refresh)))

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
                (run-with-timer 0 codex-attn-poll-interval
                                #'codex-attn--schedule-refresh))))
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
    (setq codex-attn--poll-timer nil))
  (when codex-attn--refresh-timer
    (cancel-timer codex-attn--refresh-timer)
    (setq codex-attn--refresh-timer nil)))

(defun codex-attn--blink-tick ()
  (setq codex-attn--blink-on (not codex-attn--blink-on))
  (force-mode-line-update))

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
  (when codex-attn--visible-pending
    (propertize (format " %s " codex-attn-modeline-text)
                'face (if codex-attn--blink-on
                          'codex-attn-modeline-alert-face
                        'codex-attn-modeline-idle-face)
                'mouse-face 'mode-line-highlight
                'help-echo "Agent pending. mouse-1: jump to most recent"
                'local-map codex-attn--modeline-map)))

(defun codex-attn--on-buffer-visibility-change (&rest _)
  (when codex-attn-mode
    ;; The expensive filesystem reconciliation happens only on watcher/poll
    ;; refreshes.  This hot UI hook performs a direct buffer hash lookup.
    (codex-attn--ack-visible-sessions)
    (codex-attn--update-indicator)))

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
  (codex-attn--forget-session session t)
  (codex-attn--rebuild-actionable-index)
  (codex-attn--update-indicator))

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
         (buf (codex-attn--buffer-for-session session)))
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
  (let ((status (list
                 :watch-count (length codex-attn--watches)
                 :poll-active (and codex-attn--poll-timer t)
                 :refresh-pending (and codex-attn--refresh-timer t)
                 :cache-count (hash-table-count codex-attn--state-cache)
                 :pending-count (length codex-attn--pending-sessions)
                 :actionable-count (length codex-attn--actionable-session-list)
                 :terminal-count (hash-table-count codex-attn--terminal-id->buffer)
                 :snoozed-count (hash-table-count codex-attn--snoozed-until))))
    (message "codex-attn status: watches=%d poll=%s cache=%d pending=%d actionable=%d terminals=%d snoozed=%d"
             (length codex-attn--watches)
             (if codex-attn--poll-timer "on" "off")
             (hash-table-count codex-attn--state-cache)
             (length codex-attn--pending-sessions)
             (length codex-attn--actionable-session-list)
             (hash-table-count codex-attn--terminal-id->buffer)
             (hash-table-count codex-attn--snoozed-until))
    status))

(define-minor-mode codex-attn-mode
  "Global mode for Codex/OpenCode attention indicator."
  :global t
  (if codex-attn-mode
      (progn
        (add-hook 'window-selection-change-functions #'codex-attn--on-buffer-visibility-change)
        (add-hook 'window-buffer-change-functions #'codex-attn--on-buffer-visibility-change)
        (add-to-list 'global-mode-string codex-attn--modeline-entry t)
        (codex-attn--start-watch))
    (remove-hook 'window-selection-change-functions #'codex-attn--on-buffer-visibility-change)
    (remove-hook 'window-buffer-change-functions #'codex-attn--on-buffer-visibility-change)
    (codex-attn--stop-watch)
    (codex-attn--stop-blink)
    (setq codex-attn--visible-pending nil)
    (setq global-mode-string (delete codex-attn--modeline-entry global-mode-string))
    (force-mode-line-update)))

(provide 'codex-attn)

;;; codex-attn.el ends here
