;;; codex-sessions.el --- Treemacs sidebar for agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; A standalone Treemacs tree showing live Codex/OpenCode terminal buffers and
;; highlighting those with actionable `codex-attn' notifications.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'treemacs)
(require 'treemacs-treelib)
(require 'codex-attn)

(defgroup codex-sessions nil
  "Sidebar for live Codex/OpenCode sessions."
  :group 'codex-attn)

(defcustom codex-sessions-buffer-name "*Codex Sessions*"
  "Name of the Codex sessions sidebar buffer."
  :type 'string)

(defcustom codex-sessions-side 'left
  "Frame side on which to display the sessions window."
  :type '(choice (const left) (const right)))

(defcustom codex-sessions-width 38
  "Initial width of the sessions side window."
  :type 'integer)

(defcustom codex-sessions-slot 0
  "Side-window slot used by the sessions sidebar."
  :type 'integer)

(defcustom codex-sessions-providers '(codex opencode)
  "Notifier providers displayed in the sessions sidebar.

Set this to nil to display every provider known to `codex-attn'."
  :type '(repeat symbol))

(defface codex-sessions-attention-face
  '((t :inherit success :weight bold))
  "Face for sessions that require attention.

The `success' face matches Magit's added-line green in the active theme."
  :group 'codex-sessions)

(defface codex-sessions-idle-face
  '((t :inherit shadow))
  "Face for sessions without an actionable notification."
  :group 'codex-sessions)

(defface codex-sessions-empty-face
  '((t :inherit shadow :slant italic))
  "Face used when no agent sessions are live."
  :group 'codex-sessions)

(defconst codex-sessions--empty-item 'codex-sessions--empty)
(defvar codex-sessions--refresh-timer nil)
(defvar codex-sessions--last-buffer-signature nil)

(defun codex-sessions--buffers ()
  "Return displayed agent buffers, with attention-needed buffers first."
  (let ((buffers
         (if codex-sessions-providers
             (delete-dups
              (apply #'append
                     (mapcar #'codex-attn-buffers codex-sessions-providers)))
           (codex-attn-buffers))))
    (sort buffers
          (lambda (left right)
            (let ((left-attn (codex-attn-buffer-needs-attention-p left))
                  (right-attn (codex-attn-buffer-needs-attention-p right)))
              (cond
               ((and left-attn (not right-attn)) t)
               ((and right-attn (not left-attn)) nil)
               (t (string-lessp (buffer-name left) (buffer-name right)))))))))

(defun codex-sessions--items ()
  "Return the Treemacs items representing current agent buffers."
  (or (codex-sessions--buffers)
      (list codex-sessions--empty-item)))

(defun codex-sessions--item-buffer-p (item)
  "Return non-nil when ITEM represents a live terminal buffer."
  (and (bufferp item) (buffer-live-p item)))

(defun codex-sessions--item-attention-p (item)
  "Return non-nil when ITEM represents a buffer needing attention."
  (and (codex-sessions--item-buffer-p item)
       (codex-attn-buffer-needs-attention-p item)))

(defun codex-sessions--item-icon (item)
  "Return the status icon for ITEM."
  (cond
   ((not (codex-sessions--item-buffer-p item)) "  ")
   ((codex-sessions--item-attention-p item)
    (propertize "● " 'face 'codex-sessions-attention-face))
   (t
    (propertize "○ " 'face 'codex-sessions-idle-face))))

(defun codex-sessions--item-label (item)
  "Return the display label for ITEM."
  (if (codex-sessions--item-buffer-p item)
      (propertize
       (codex-sessions--display-name item)
       'face (if (codex-sessions--item-attention-p item)
                 'codex-sessions-attention-face
               'default))
    (propertize "No active agent sessions" 'face 'codex-sessions-empty-face)))

(defun codex-sessions--display-name (buffer)
  "Return BUFFER's name without its redundant notifier decoration."
  (let* ((name (buffer-name buffer))
         (provider (codex-attn-buffer-provider buffer))
         (prefix (and provider
                      (codex-attn-provider-buffer-prefix provider)))
         (label (if (and prefix (string-prefix-p prefix name))
                    (substring name (length prefix))
                  name)))
    ;; Emacs puts uniquifying suffixes after the closing star, for example
    ;; "*codex: project*<2>".  Remove only the decoration, retaining "<2>".
    (replace-regexp-in-string "\\*\\(<[0-9]+>\\)?\\'" "\\1" label)))

(defun codex-sessions--item-key (item)
  "Return a stable tree key for ITEM."
  (if (codex-sessions--item-buffer-p item)
      item
    codex-sessions--empty-item))

(defun codex-sessions--item-properties (item)
  "Return additional Treemacs properties for ITEM."
  (when (codex-sessions--item-buffer-p item)
    (list :buffer item
          :provider (codex-attn-buffer-provider item)
          'help-echo "RET or mouse-1: visit this session")))

(defun codex-sessions--buffer-at-point ()
  "Return the live session buffer represented by the node at point."
  (let* ((button (treemacs-current-button))
         (buffer (and button (treemacs-button-get button :buffer))))
    (and (buffer-live-p buffer) buffer)))

(defun codex-sessions-visit (&optional _prefix)
  "Visit the session represented by the node at point."
  (interactive "P")
  (if-let ((buffer (codex-sessions--buffer-at-point)))
      (pop-to-buffer buffer)
    (message "codex-sessions: no live session on this row.")))

(defun codex-sessions-visit-button (button)
  "Visit the session stored on Treemacs BUTTON."
  (let ((buffer (and button (treemacs-safe-button-get button :buffer))))
    (if (buffer-live-p buffer)
        (pop-to-buffer buffer)
      (message "codex-sessions: this session is no longer live.")
      (codex-sessions--schedule-refresh))))

(defun codex-sessions-visit-mouse (event)
  "Visit the session clicked in mouse EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (point (posn-point position)))
    (when (and (window-live-p window) (integer-or-marker-p point))
      (select-window window)
      (goto-char point)
      (codex-sessions-visit))))

(defun codex-sessions--row-positions ()
  "Return buffer positions for all visible session tree rows."
  (let ((position (point-min))
        button
        rows)
    (while (setq button (next-button position))
      (let ((button-position (if (markerp button)
                                 (marker-position button)
                               button)))
        (when (= 0 (or (treemacs-button-get button :depth) -1))
          (push button-position rows))
        (setq position (1+ button-position))))
    (nreverse rows)))

(defun codex-sessions--move-row (direction)
  "Move to the next session row in DIRECTION, wrapping at either end."
  (let* ((rows (codex-sessions--row-positions))
         (button (treemacs-current-button))
         (current (cond
                   ((markerp button) (marker-position button))
                   ((integer-or-marker-p button) button)
                   (t (point))))
         (target
          (pcase direction
            ('next (or (seq-find (lambda (position) (> position current)) rows)
                       (car rows)))
            ('previous
             (or (car (last (seq-filter
                             (lambda (position) (< position current))
                             rows)))
                 (car (last rows)))))))
    (when target
      (goto-char target)
      (treemacs--evade-image)
      (hl-line-highlight)
      (when-let ((window (get-buffer-window (current-buffer) t)))
        (set-window-point window (point))))))

(defun codex-sessions-next-line ()
  "Select the next session row, wrapping to the first after the last."
  (interactive)
  (codex-sessions--move-row 'next))

(defun codex-sessions-previous-line ()
  "Select the previous session row, wrapping to the last before the first."
  (interactive)
  (codex-sessions--move-row 'previous))

(treemacs-define-leaf-node-type codex-sessions-session
  :icon (codex-sessions--item-icon item)
  :label (codex-sessions--item-label item)
  :key (codex-sessions--item-key item)
  :more-properties (codex-sessions--item-properties item)
  :ret-action #'codex-sessions-visit
  :visit-action #'codex-sessions-visit-button
  :double-click-action #'codex-sessions-visit)

(treemacs-define-variadic-entry-node-type codex-sessions-tree
  :key 'codex-sessions-tree
  :children (codex-sessions--items)
  :child-type 'codex-sessions-session)

(defun codex-sessions--buffer-signature ()
  "Return the live buffer/name signature relevant to the sidebar."
  (mapcar (lambda (buffer) (cons buffer (buffer-name buffer)))
          (codex-sessions--buffers)))

(defun codex-sessions--update-header ()
  "Update the sidebar header from current attention state."
  (let* ((buffers (codex-sessions--buffers))
         (attention-count
          (seq-count #'codex-attn-buffer-needs-attention-p buffers)))
    (setq-local
     header-line-format
     (list " Codex Sessions  "
           (propertize (format "● %d" attention-count)
                       'face (if (> attention-count 0)
                                 'codex-sessions-attention-face
                               'codex-sessions-idle-face))
           (format "  ○ %d" (- (length buffers) attention-count))))))

(defun codex-sessions--cleanup-buffer ()
  "Clean up timers associated with the sidebar buffer."
  (when codex-sessions--refresh-timer
    (cancel-timer codex-sessions--refresh-timer)
    (setq codex-sessions--refresh-timer nil))
  (setq codex-sessions--last-buffer-signature nil))

(defun codex-sessions--configure-buffer ()
  "Apply sidebar-specific behavior after `treemacs-initialize'."
  (setq-local mode-line-format nil)
  (setq-local truncate-lines t)
  (setq-local window-size-fixed 'width)
  (add-hook 'kill-buffer-hook #'codex-sessions--cleanup-buffer nil t)
  (let ((map (copy-keymap treemacs-mode-map)))
    (define-key map (kbd "g") #'codex-sessions-refresh)
    (define-key map (kbd "q") #'codex-sessions-quit)
    (dolist (key '("n" "<down>" "C-n"))
      (define-key map (kbd key) #'codex-sessions-next-line))
    (dolist (key '("p" "<up>" "C-p"))
      (define-key map (kbd key) #'codex-sessions-previous-line))
    (define-key map [mouse-1] #'codex-sessions-visit-mouse)
    (use-local-map map))
  (codex-sessions--update-header))

(defun codex-sessions--initialize-buffer (buffer)
  "Initialize BUFFER as the sessions Treemacs tree."
  (with-current-buffer buffer
    (treemacs-initialize codex-sessions-tree
      :and-do (codex-sessions--configure-buffer))))

(defun codex-sessions--restore-selection (buffer)
  "Restore point to the tree row representing BUFFER, when present."
  (when (buffer-live-p buffer)
    (when-let ((position
                (text-property-any (point-min) (point-max) :buffer buffer)))
      (goto-char position)
      (when-let ((window (get-buffer-window (current-buffer) t)))
        (set-window-point window position)))))

(defun codex-sessions--redraw ()
  "Redraw the sidebar from current in-memory notifier state."
  (when codex-sessions--refresh-timer
    (cancel-timer codex-sessions--refresh-timer)
    (setq codex-sessions--refresh-timer nil))
  (when-let ((buffer (get-buffer codex-sessions-buffer-name)))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        ;; A notification changing state can also reorder the rows.  Rebuilding
        ;; this small tree is more reliable than incrementally collapsing and
        ;; reopening Treemacs' invisible variadic root after focus has moved.
        (let ((selected-buffer (codex-sessions--buffer-at-point)))
          (codex-sessions--initialize-buffer buffer)
          (codex-sessions--restore-selection selected-buffer))
        (setq codex-sessions--last-buffer-signature
              (codex-sessions--buffer-signature))))))

(defun codex-sessions--schedule-refresh (&rest _)
  "Coalesce a burst of notifier changes into one sidebar redraw."
  (when (and (get-buffer-window codex-sessions-buffer-name t)
             (not codex-sessions--refresh-timer))
    (setq codex-sessions--refresh-timer
          (run-at-time 0 nil #'codex-sessions--redraw))))

(defun codex-sessions--maybe-refresh-buffer-list ()
  "Refresh when a displayed session buffer is created, killed, or renamed."
  (when (and (get-buffer-window codex-sessions-buffer-name t)
             (not (equal codex-sessions--last-buffer-signature
                         (codex-sessions--buffer-signature))))
    (codex-sessions--schedule-refresh)))

(defun codex-sessions-refresh ()
  "Refresh notifier state and redraw the sessions sidebar."
  (interactive)
  (codex-attn-refresh)
  (codex-sessions--redraw))

(defun codex-sessions-show ()
  "Show and select the Codex sessions side window."
  (interactive)
  (codex-attn-refresh)
  (let* ((buffer (get-buffer-create codex-sessions-buffer-name))
         (window
          (display-buffer-in-side-window
           buffer
           `((side . ,codex-sessions-side)
             (slot . ,codex-sessions-slot)
             (window-width . ,codex-sessions-width)
             (dedicated . t)))))
    (codex-sessions--initialize-buffer buffer)
    (setq codex-sessions--last-buffer-signature
          (codex-sessions--buffer-signature))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)
    (select-window window)))

(defun codex-sessions-quit ()
  "Close the sessions side window without killing its buffer."
  (interactive)
  (when-let ((window (get-buffer-window codex-sessions-buffer-name)))
    (delete-window window)))

(defun codex-sessions-toggle ()
  "Toggle the Codex sessions side window."
  (interactive)
  (if (get-buffer-window codex-sessions-buffer-name)
      (codex-sessions-quit)
    (codex-sessions-show)))

(add-hook 'codex-attn-state-change-hook #'codex-sessions--schedule-refresh)
(add-hook 'buffer-list-update-hook #'codex-sessions--maybe-refresh-buffer-list)

(provide 'codex-sessions)

;;; codex-sessions.el ends here
