;;; codex-chatty.el --- Spoken discussion beside a working Codex -*- lexical-binding: t; -*-

;;; Commentary:
;; A thin Emacs adapter.  The Python controller owns conversation and audio state.
;; M-x codex-chatty toggles recording; C-c C-c does the same in the transcript.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'codex-voice)
(require 'codex-app-server)

(defgroup codex-chatty nil "Conversational voice alongside Codex." :group 'tools)
(defcustom codex-chatty-python-command nil
  "Python command prefix.  Nil reuses `codex-app-server-python-command'."
  :type '(choice (const nil) (repeat string)))
(defcustom codex-chatty-tts-options nil
  "Additional Kokoro/player settings as an alist.  Normally use the approved defaults."
  :type 'alist)
(defcustom codex-chatty-whisper-step 2
  "Seconds between incremental decodes in Chatty; ordinary dictation is unaffected."
  :type 'number)
(defcustom codex-chatty-record-on-open t
  "Start the microphone when opening Chatty.  Nil opens a ready discussion only."
  :type 'boolean)
(defconst codex-chatty--script
  (expand-file-name "codex-chatty.py" (file-name-directory (or load-file-name buffer-file-name))))
(defvar codex-chatty--session nil)

(defun codex-chatty--put (session key value)
  "Mutate shared SESSION without replacing its plist head."
  (if-let* ((cell (plist-member session key)))
      (setcar (cdr cell) value)
    (nconc session (list key value)))
  value)

(defvar codex-chatty-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-c C-c") #'codex-chatty)
    (define-key map (kbd "C-c C-k") #'codex-chatty-close)
    (define-key map (kbd "q") #'codex-chatty-close)
    map))

(define-derived-mode codex-chatty-mode special-mode "Chatty"
  "Transcript of a voice discussion; C-c C-c starts or stops a recording."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'codex-chatty--transcript-killed nil t))

(defun codex-chatty--active-p ()
  (and codex-chatty--session
       (process-live-p (plist-get codex-chatty--session :process))
       (not (plist-get codex-chatty--session :closed))))

(defun codex-chatty--append (session text)
  (when-let* ((buffer (plist-get session :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (follow (= (point) (point-max))))
          (save-excursion (goto-char (point-max)) (insert text))
          (when follow
            (goto-char (point-max))
            (when-let* ((window (get-buffer-window buffer t)))
              (set-window-point window (point-max)))))))))

(defun codex-chatty--state (session state)
  (codex-chatty--put session :state state)
  (when (buffer-live-p (plist-get session :buffer))
    (with-current-buffer (plist-get session :buffer)
      (setq header-line-format
            (format " Chatty: %s  |  C-c C-c: record/stop  |  C-c C-k: close" state)))))

(defun codex-chatty--send (session command &rest fields)
  (when (process-live-p (plist-get session :process))
    (process-send-string
     (plist-get session :process)
     (concat (json-encode
              (append `((command . ,command) (session_id . ,(plist-get session :id))) fields))
             "\n"))))

(defun codex-chatty--event (process event)
  (let ((session (process-get process 'codex-chatty-session)))
    (when (and (eq session codex-chatty--session)
               (equal (plist-get event :session_id) (plist-get session :id)))
      (pcase (plist-get event :event)
        ("recording"
         (codex-chatty--put session :recording t)
         (codex-chatty--state session "Opening microphone"))
        ("capture-ready" (codex-chatty--state session "Listening"))
        ("transcribing"
         (codex-chatty--put session :recording nil)
         (codex-chatty--state session "Transcribing"))
        ("thinking" (unless (equal (plist-get session :state) "Speaking")
                      (codex-chatty--state session "Thinking")))
        ("opened" (codex-chatty--put session :log (plist-get event :log)))
        ("ready" (when (equal (plist-get session :state) "Opening")
                   (codex-chatty--state session "Ready")))
        ("user-message"
         (codex-chatty--put session :recording nil)
         (codex-chatty--put session :last-item nil)
         (codex-chatty--append session (concat "\nYou: " (plist-get event :text) "\n"))
         (codex-chatty--state session "Thinking"))
        ("assistant-delta"
         (unless (equal (plist-get session :last-item) (plist-get event :item_id))
           (codex-chatty--put session :last-item (plist-get event :item_id))
           (codex-chatty--append session "\nChatty: "))
         (codex-chatty--append session (plist-get event :text)))
        ("first-audio" (codex-chatty--state session "Speaking"))
        ("interrupted"
         (codex-chatty--append session "\n[Response interrupted; some text above may not have been spoken.]\n"))
        ("idle" (unless (equal (plist-get session :state) "Listening")
                  (codex-chatty--state session "Ready")))
        ("handoff-preparing"
         (codex-chatty--state session "Sending instruction")
         (codex-chatty--append session
                               (concat "\n\nInstruction for working Codex:\n"
                                       (plist-get event :instruction) "\n")))
        ("handoff"
         (codex-chatty--append session
                               (format "\nHandoff: %s%s\n" (plist-get event :status)
                                       (if-let* ((error (plist-get event :error)))
                                           (concat " — " error) ""))))
        ("acknowledgement"
         (codex-chatty--append session (concat "\nChatty: " (plist-get event :text) "\n")))
        ("status" (codex-chatty--append session (concat "\n" (plist-get event :text) "\n")))
        ((or "error" "fatal" "transcription-error" "speech-error")
         (when (equal (plist-get event :event) "transcription-error")
           (codex-chatty--put session :recording nil))
         (codex-chatty--append session (concat "\n" (plist-get event :message) "\n"))
         (when-let* ((draft (plist-get event :transcript)))
           (codex-chatty--append session (concat "Unsent transcription: " draft "\n")))
         (codex-chatty--state session "Error — see transcript")
         (message "Chatty: %s" (plist-get event :message)))
        ("closed"
         (codex-chatty--put session :closed t)
         (codex-chatty--state session "Closed")
         (codex-chatty--append session (format "\n[%s]\n" (plist-get event :reason))))))))

(defun codex-chatty--filter (process output)
  (let* ((pending (concat (or (process-get process 'codex-chatty-pending) "") output))
         (start 0))
    (while (string-match "\n" pending start)
      (let ((line (substring pending start (match-beginning 0))))
        (setq start (match-end 0))
        (unless (string-empty-p line)
          (condition-case err
              (let ((json-object-type 'plist) (json-array-type 'list)
                    (json-key-type 'keyword) (json-false nil))
                (codex-chatty--event process (json-read-from-string line)))
            (error (message "Chatty event error: %s" (error-message-string err)))))))
    (process-put process 'codex-chatty-pending (substring pending start))))

(defun codex-chatty--sentinel (process _event)
  (when (memq (process-status process) '(exit signal))
    (let ((session (process-get process 'codex-chatty-session)))
      (when (eq session codex-chatty--session)
        (unless (plist-get session :closed)
          (codex-chatty--put session :closed t)
          (codex-chatty--state session "Closed")
          (codex-chatty--append
           session (format "\n[Controller exited: %s. Log: %s]\n"
                           (process-exit-status process) (or (plist-get session :log) "*Chatty Log*"))))))))

(defun codex-chatty--target-killed ()
  (when (eq (current-buffer) (plist-get codex-chatty--session :target))
    (codex-chatty-close)))

(defun codex-chatty--transcript-killed ()
  (when (eq (current-buffer) (plist-get codex-chatty--session :buffer))
    (codex-chatty-close)))

(defun codex-chatty--whisper-config ()
  (require 'whisper)
  (let* ((command (whisper-command ""))
         (server (whisper--find-whispercpp-server))
         (model (cadr (member "--model" command)))
         (vad (cadr (member "--vad-model" command))))
    (unless (and (file-executable-p server) (file-readable-p model) (file-readable-p vad)
                 (executable-find "ffmpeg"))
      (user-error "Chatty requires the configured local Whisper/Silero models and FFmpeg"))
    `((server . ,server) (model . ,model) (vad_model . ,vad)
      (threads . ,(or whisper-use-threads 4)) (input_format . ,whisper--ffmpeg-input-format)
      (input_device . ,whisper--ffmpeg-input-device) (step . ,codex-chatty-whisper-step)
      (window . 30) (overlap . 3))))

(defun codex-chatty--check-microphone-owner ()
  (when (or (and (fboundp 'whisper-recording-p) (whisper-recording-p))
            (and (fboundp 'whisper-transcribing-p) (whisper-transcribing-p)))
    (user-error "Finish the existing dictation before starting Chatty")))

(defun codex-chatty--guard-other-recording (&rest _)
  (when (and (codex-chatty--active-p)
             (or (plist-get codex-chatty--session :recording)
                 (member (plist-get codex-chatty--session :state) '("Listening" "Transcribing"))))
    (user-error "Chatty owns the microphone; finish or close its recording first")))

(with-eval-after-load 'whisper
  (advice-add 'whisper-run :before #'codex-chatty--guard-other-recording))

(defun codex-chatty--start (target)
  (unless (and (buffer-live-p target)
               (with-current-buffer target (derived-mode-p 'ghostel-mode))
               (eq (codex-attn-buffer-provider target) 'codex)
               (codex-voice--thread-id target))
    (user-error "Open Chatty from the intended working Codex terminal"))
  (codex-chatty--check-microphone-owner)
  (let* ((context (codex-voice--read-context target))
         (whisper (codex-chatty--whisper-config))
         (thread (codex-voice--thread-id target))
         (id (format "chatty-%s-%s" (emacs-pid) (format-time-string "%s%N")))
         (buffer (generate-new-buffer "*Chatty Codex*"))
         (logs (get-buffer-create "*Chatty Log*"))
         (session (list :id id :target target :thread thread :buffer buffer :state "Opening"))
         process)
    (with-current-buffer buffer
      (codex-chatty-mode)
      (let ((inhibit-read-only t))
        (insert (format "Chatty Codex — working thread %s\nSpanish input · English speech\n" thread))))
    (setq codex-chatty--session session)
    (codex-chatty--state session "Opening")
    (condition-case err
        (progn
          ;; Server startup and microphone/model startup proceed independently.
          (codex-app-server-ensure #'ignore
                                   (lambda (error) (message "Chatty server: %s" error)))
          (setq process
                (make-process :name "codex-chatty" :buffer nil :stderr logs :noquery t
                              :coding 'utf-8-unix :connection-type 'pipe
                              :command (append (or codex-chatty-python-command
                                                   codex-app-server-python-command)
                                               (list codex-chatty--script))
                              :filter #'codex-chatty--filter :sentinel #'codex-chatty--sentinel))
          (codex-chatty--put session :process process)
          (process-put process 'codex-chatty-session session)
          (process-send-string
           process (concat (json-encode
                            `((command . "open") (session_id . ,id)
                              (working_thread_id . ,thread) (context . ,context)
                              (cwd . ,(with-current-buffer target (expand-file-name default-directory)))
                              (endpoint . ,codex-app-server-endpoint)
                              (auto_record . ,(if codex-chatty-record-on-open t json-false))
                              (whisper . ,whisper) (tts . ,(or codex-chatty-tts-options (make-hash-table)))))
                           "\n"))
          (with-current-buffer target
            (add-hook 'kill-buffer-hook #'codex-chatty--target-killed nil t))
          (display-buffer buffer))
      (error
       (codex-chatty--put session :closed t)
       (when (process-live-p process) (delete-process process))
       (codex-chatty--state session "Failed to start")
       (signal (car err) (cdr err))))))

;;;###autoload
(defun codex-chatty ()
  "Start a voice discussion, or toggle recording in the active discussion."
  (interactive)
  (if (codex-chatty--active-p)
      (let ((thread (codex-voice--thread-id (current-buffer))))
        (when (and thread (not (equal thread (plist-get codex-chatty--session :thread))))
          (user-error "Chatty is bound to another Codex thread; close it before opening this one"))
        (if (or (plist-get codex-chatty--session :recording)
                (equal (plist-get codex-chatty--session :state) "Listening"))
            (progn (codex-chatty--state codex-chatty--session "Transcribing")
                   (codex-chatty--put codex-chatty--session :recording nil)
                   (codex-chatty--send codex-chatty--session "stop_recording"))
          (codex-chatty--check-microphone-owner)
          (codex-chatty--send codex-chatty--session "start_recording")))
    (codex-chatty--start
     (if (and (derived-mode-p 'codex-chatty-mode)
              (buffer-live-p (plist-get codex-chatty--session :target)))
         (plist-get codex-chatty--session :target)
       (current-buffer)))))

;;;###autoload
(defun codex-chatty-close ()
  "Close Chatty without submitting anything or stopping working Codex."
  (interactive)
  (when (codex-chatty--active-p)
    (codex-chatty--send codex-chatty--session "close")
    (codex-chatty--state codex-chatty--session "Closing")))

(add-hook 'kill-emacs-hook #'codex-chatty-close)

(provide 'codex-chatty)
;;; codex-chatty.el ends here
