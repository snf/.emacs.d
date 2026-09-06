;;; markdown-table-display.el --- Copyable wrapped Markdown tables -*- lexical-binding: t; -*-

;;; Commentary:

;; Show a read-only Markdown view whose tables are real, selectable text and
;; whose cells wrap to the available window width.  The file-visiting source
;; buffer remains unchanged and writable.  `markdown-table-wrap' supplies the
;; table layout algorithm.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'markdown-table-wrap)
(require 'subr-x)

(defgroup markdown-table-display nil
  "Width-aware textual rendering of Markdown pipe tables."
  :group 'markdown)

(defcustom markdown-table-display-default-width 100
  "Width used when neither source nor view is visible in a window."
  :type 'integer
  :group 'markdown-table-display)

(defcustom markdown-table-display-width-padding 1
  "Columns kept free at the right edge of the narrowest visible window."
  :type 'integer
  :group 'markdown-table-display)

(defcustom markdown-table-display-refresh-delay 0.15
  "Idle delay in seconds before refreshing the view after an edit."
  :type 'number
  :group 'markdown-table-display)

(defcustom markdown-table-display-max-cell-height nil
  "Maximum displayed cell height, or nil to show all cell text."
  :type '(choice (const :tag "Unlimited" nil) positive-integer)
  :group 'markdown-table-display)

(defvar markdown-table-display--inhibit-auto-enable nil)
(defvar markdown-table-display-mode)
(defvar-local markdown-table-display--view-buffer nil)
(defvar-local markdown-table-display--source-buffer nil)
(defvar-local markdown-table-display--refresh-timer nil)
(defvar-local markdown-table-display--last-width nil)
(defvar-local markdown-table-display--source-point nil)
(defvar-local markdown-table-display--refreshing nil)

(defconst markdown-table-display--line-regexp "^.*|.*$")
(defconst markdown-table-display--separator-regexp
  "^[ \t]*|?[ \t]*:?-+:?[ \t]*\\(?:|[ \t]*:?-+:?[ \t]*\\)+|?[ \t]*$")

(defvar markdown-table-display-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'markdown-table-display-open-link)
    (define-key map [mouse-2] #'markdown-table-display-open-link)
    ;; Let Emacs distinguish a mouse-1 click from a drag, so dragging across a
    ;; link still selects text while an ordinary click follows it.
    (define-key map [follow-link] #'markdown-table-display-open-link)
    map)
  "Keymap placed on links in a generated table view.")

(defun markdown-table-display--preserve-link-properties ()
  "Keep generated link interaction properties across fontification."
  (setq-local font-lock-defaults (copy-tree font-lock-defaults))
  (when-let* ((entry (assq 'font-lock-extra-managed-props
                           font-lock-defaults)))
    (setcdr entry (cl-set-difference
                   (cdr entry) '(keymap help-echo mouse-face))))
  ;; This variable is populated lazily from `font-lock-defaults'.  Update it as
  ;; well when the view has already been fontified before a module reload.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(keymap help-echo mouse-face))))

(define-derived-mode markdown-table-display-view-mode markdown-mode
  "Markdown-Table-View"
  "Major mode for a read-only, copyable Markdown table view."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function
              #'markdown-table-display--revert-from-source)
  (setq-local header-line-format
              " Copyable table view — q: close; r: reload disk; RET/click: open link; C-c |: edit source")
  (markdown-table-display--preserve-link-properties)
  (visual-line-mode 1))

(define-key markdown-table-display-view-mode-map (kbd "r") #'revert-buffer)
(define-key markdown-table-display-view-mode-map (kbd "q")
  #'markdown-table-display-quit)

(defun markdown-table-display-open-link (&optional event)
  "Open the generated table link at point or mouse EVENT."
  (interactive (list last-command-event))
  (when (mouse-event-p event)
    (posn-set-point (event-end event)))
  (let ((url (or (get-char-property (point) 'markdown-table-url)
                 (and (> (point) (point-min))
                      (get-char-property (1- (point))
                                         'markdown-table-url)))))
    (unless url
      (user-error "No link at point"))
    (markdown--browse-url url)))

(defun markdown-table-display--add-face (string face)
  "Add FACE to STRING using a font-lock-resistant text property."
  (let ((position 0)
        (length (length string)))
    (while (< position length)
      (let* ((next (or (next-single-property-change
                        position 'font-lock-face string)
                       length))
             (existing (get-text-property position 'font-lock-face string))
             (combined (cond
                        ((null existing) face)
                        ((listp existing) (cons face existing))
                        (t (list face existing)))))
        (put-text-property position next 'font-lock-face combined string)
        (setq position next))))
  string)

(defun markdown-table-display--link-text (label url)
  "Return styled LABEL carrying an actionable Markdown link to URL."
  (let ((label (markdown-table-display--style-emphasis label)))
    (markdown-table-display--add-face label 'markdown-link-face)
    (add-text-properties
     0 (length label)
     `(markdown-table-url ,url
       keymap ,markdown-table-display-link-map
       mouse-face highlight
       follow-link t
       help-echo ,url
       rear-nonsticky t)
     label)
    label))

(defun markdown-table-display--style-emphasis (text)
  "Remove emphasis markup from TEXT and add equivalent face properties."
  (let ((result text))
    (dolist (entry '(("\\*\\*\\*\\([^*\n]+\\)\\*\\*\\*"
                      (markdown-bold-face markdown-italic-face))
                     ("\\*\\*\\([^*\n]+\\)\\*\\*" markdown-bold-face)
                     ("~~\\([^~\n]+\\)~~" markdown-strike-through-face)
                     ("\\*\\([^*\n]+\\)\\*" markdown-italic-face)))
      (pcase-let ((`(,regexp ,face) entry))
        (setq result
              (replace-regexp-in-string
               regexp
               (lambda (match)
                 (markdown-table-display--add-face
                  (match-string 1 match) face))
               result t t))))
    result))

(defun markdown-table-display--style-inline-markup (text)
  "Turn inline Markdown in TEXT into styled, actionable plain text."
  (let ((result text)
        (protected nil)
        (index 0))
    (cl-labels
        ((protect
          (regexp converter)
          (setq result
                (replace-regexp-in-string
                 regexp
                 (lambda (match)
                   (let ((placeholder (format "\x00MTD%d\x00" index)))
                     (push (cons placeholder (funcall converter match)) protected)
                     (setq index (1+ index))
                     placeholder))
                 result t t))))
      ;; Protect code before processing emphasis inside other constructs.
      (protect
       "``\\([^`\n]\\|`[^`\n]\\)+``"
       (lambda (match)
         (let ((inner (substring match 2 -2)))
           (when (and (> (length inner) 1)
                      (string-prefix-p " " inner)
                      (string-suffix-p " " inner))
             (setq inner (substring inner 1 -1)))
           (markdown-table-display--add-face inner
                                             'markdown-inline-code-face))))
      (protect
       "`\\([^`\n]+\\)`"
       (lambda (match)
         (markdown-table-display--add-face
          (substring match 1 -1) 'markdown-inline-code-face)))
      ;; Some generated reports use an empty pair of backticks to denote an
      ;; empty byte string.  Consume it before any later markup processing.
      (protect "``" (lambda (_match) ""))
      (protect
       markdown-regex-link-inline
       (lambda (match)
         (markdown-table-display--link-text
          (match-string 3 match) (match-string 6 match))))
      (protect
       "<\\(https?://[^>]+\\)>"
       (lambda (match)
         (markdown-table-display--link-text
          (match-string 1 match) (match-string 1 match))))
      (protect
       "https?://[^][()<> \\t\\n|]+"
       (lambda (match)
         (markdown-table-display--link-text match match)))
      (setq result (markdown-table-display--style-emphasis result))
      (dolist (item protected)
        (setq result
              (replace-regexp-in-string
               (regexp-quote (car item)) (cdr item) result t t))))
    result))

(defun markdown-table-display--source-buffer ()
  "Return the source buffer associated with the current buffer."
  (if (derived-mode-p 'markdown-table-display-view-mode)
      markdown-table-display--source-buffer
    (current-buffer)))

(defun markdown-table-display--revert-from-source (ignore-auto noconfirm)
  "Reload the source from disk and regenerate this table view.

IGNORE-AUTO and NOCONFIRM have the same meanings as for `revert-buffer'."
  (let ((source markdown-table-display--source-buffer))
    (unless (buffer-live-p source)
      (user-error "The source buffer no longer exists"))
    (with-current-buffer source
      ;; Preserve the source's major and minor modes, including its association
      ;; with this generated view.  `revert-buffer' still protects unsaved
      ;; source edits unless NOCONFIRM is non-nil.
      (revert-buffer ignore-auto noconfirm t))
    (markdown-table-display-refresh)
    t))

(defun markdown-table-display--visible-width (source)
  "Return a width fitting every live window associated with SOURCE."
  (let* ((view (buffer-local-value 'markdown-table-display--view-buffer source))
         (windows (append (get-buffer-window-list source nil t)
                          (and (buffer-live-p view)
                               (get-buffer-window-list view nil t)))))
    (max 20
         (- (if windows
                (apply #'min (mapcar #'window-body-width windows))
              markdown-table-display-default-width)
            markdown-table-display-width-padding))))

(defun markdown-table-display--bounds-at-separator (separator)
  "Return table bounds around SEPARATOR, or nil when there is no header."
  (save-excursion
    (goto-char separator)
    (if (bobp)
        nil
      (forward-line -1)
      (let ((beginning (line-beginning-position)))
        (if (not (looking-at-p markdown-table-display--line-regexp))
            nil
          (forward-line 2)
          (while (and (not (eobp))
                      (looking-at-p markdown-table-display--line-regexp))
            (forward-line 1))
          (cons beginning (point)))))))

(defun markdown-table-display--normalize-outer-pipes (text)
  "Add optional outer pipes to every table row in TEXT."
  (mapconcat
   (lambda (line)
     (let ((line (string-trim line)))
       (unless (string-prefix-p "|" line)
         (setq line (concat "|" line)))
       (unless (string-suffix-p "|" line)
         (setq line (concat line "|")))
       line))
   (split-string text "\n") "\n"))

(defun markdown-table-display--render-rows (text width)
  "Render pipe-table TEXT into one display string per source row at WIDTH."
  ;; Convert syntax to visible text properties before layout so URL targets do
  ;; not steal column width while formatting and link behavior survive wraps.
  (let ((plain (markdown-table-display--normalize-outer-pipes
                (markdown-table-display--style-inline-markup text))))
    (pcase-let* ((`(,headers ,aligns ,rows)
                   (markdown-table-wrap-parse plain))
                 (column-count (length headers)))
      (if (or (= column-count 0)
              (< (- width (+ (* 3 column-count) 1)) column-count))
          (split-string plain "\n")
        (let* ((metrics (markdown-table-wrap-compute-table-metrics
                         headers rows column-count))
               (column-widths (markdown-table-wrap-distribute-widths
                               metrics width column-count)))
          (append
           (list (string-join
                  (markdown-table-wrap--render-row
                   headers column-widths aligns
                   markdown-table-display-max-cell-height)
                  "\n")
                 (markdown-table-wrap--render-separator
                  column-widths aligns))
           (mapcar
            (lambda (row)
              (string-join
               (markdown-table-wrap--render-row
                row column-widths aligns markdown-table-display-max-cell-height)
               "\n"))
            rows)))))))

(defun markdown-table-display--render-table
    (beginning end width &optional source-offset)
  "Render table between BEGINNING and END as copyable text at WIDTH.

SOURCE-OFFSET is added to positions recorded for navigation back to the
source buffer."
  (let* ((ends-in-newline (and (> end beginning) (eq (char-before end) ?\n)))
         (text-end (if ends-in-newline (1- end) end))
         (source (buffer-substring-no-properties beginning text-end))
         (rows (markdown-table-display--render-rows source width))
         (position beginning)
         (source-offset (or source-offset 0))
         (rendered nil))
    (dolist (row rows)
      ;; This property makes C-c | return to the corresponding source row.
      (push (propertize row 'markdown-table-source-position
                        (+ source-offset position))
            rendered)
      (setq position (save-excursion
                       (goto-char position)
                       (min end (line-beginning-position 2)))))
    (concat (string-join (nreverse rendered) "\n")
            (and ends-in-newline "\n"))))

(defun markdown-table-display--render-verbatim
    (beginning end &optional source-offset)
  "Return text from BEGINNING to END with source-line navigation properties.

SOURCE-OFFSET is added to the recorded buffer positions."
  (let ((position beginning)
        (source-offset (or source-offset 0))
        pieces)
    (while (< position end)
      (let ((next (save-excursion
                    (goto-char position)
                    (min end (line-beginning-position 2)))))
        (push (propertize
               (buffer-substring-no-properties position next)
               'markdown-table-source-position (+ source-offset position)
               'markdown-table-source-verbatim t)
              pieces)
        (setq position next)))
    (apply #'concat (nreverse pieces))))

(defun markdown-table-display--render-buffer-string
    (text width &optional source-offset)
  "Return TEXT with real, copyable tables reflowed to WIDTH.

SOURCE-OFFSET is added to positions recorded for source navigation."
  (with-temp-buffer
    (insert text)
    (let ((pieces nil)
          (last (point-min))
          (source-offset (or source-offset 0)))
      (goto-char (point-min))
      (while (re-search-forward markdown-table-display--separator-regexp nil t)
        (let* ((separator (line-beginning-position))
               (bounds (markdown-table-display--bounds-at-separator separator)))
          (cond
           ((not bounds)
            (forward-line 1))
           ((markdown-table-wrap-inside-code-fence-p (car bounds))
            (goto-char (cdr bounds)))
           (t
            (push (markdown-table-display--render-verbatim
                   last (car bounds) source-offset)
                  pieces)
            (push (markdown-table-display--render-table
                   (car bounds) (cdr bounds) width source-offset)
                  pieces)
            (setq last (cdr bounds))
            (goto-char last)))))
      (push (markdown-table-display--render-verbatim
             last (point-max) source-offset)
            pieces)
      (apply #'concat (nreverse pieces)))))

(defun markdown-table-display--property-at (position property)
  "Return PROPERTY at POSITION, checking the preceding character as fallback."
  (or (get-text-property position property)
      (and (> position (point-min))
           (get-text-property (1- position) property))))

(defun markdown-table-display--view-position-for-source
    (view source source-position)
  "Return the position in VIEW corresponding to SOURCE-POSITION in SOURCE."
  (let (source-line source-column)
    (with-current-buffer source
      (save-excursion
        (goto-char (min (max source-position (point-min)) (point-max)))
        (setq source-line (line-beginning-position)
              source-column (current-column))))
    (with-current-buffer view
      (let ((position (text-property-any
                       (point-min) (point-max)
                       'markdown-table-source-position source-line)))
        (if (not position)
            (point-max)
          (save-excursion
            (goto-char position)
            ;; Verbatim lines have a one-to-one horizontal mapping.  A table
            ;; row is only approximate because markup is hidden and cells may
            ;; have wrapped, but retaining its source column is still a useful
            ;; placement within the corresponding rendered row.
            (move-to-column source-column)
            (point)))))))

(defun markdown-table-display--source-position-for-view
    (source view-position)
  "Return the position in SOURCE corresponding to VIEW-POSITION."
  (let (source-line view-column)
    (save-excursion
      (goto-char (min (max view-position (point-min)) (point-max)))
      (setq source-line
            (markdown-table-display--property-at
             (point) 'markdown-table-source-position)
            view-column (current-column)))
    (when source-line
      (with-current-buffer source
        (save-excursion
          (goto-char (min (max source-line (point-min)) (point-max)))
          (move-to-column view-column)
          (point))))))

(defun markdown-table-display--view-killed ()
  "Forget this view in its associated source buffer."
  (let ((source markdown-table-display--source-buffer)
        (view (current-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (eq markdown-table-display--view-buffer view)
          (setq markdown-table-display--view-buffer nil))))))

(defun markdown-table-display--ensure-view (source)
  "Return SOURCE's live table view, creating it when necessary."
  (or (and (buffer-live-p markdown-table-display--view-buffer)
           markdown-table-display--view-buffer)
      (let ((view (generate-new-buffer
                   (format "*Table view: %s*" (buffer-name source)))))
        (setq markdown-table-display--view-buffer view)
        (with-current-buffer view
          (let ((markdown-table-display--inhibit-auto-enable t))
            (markdown-table-display-view-mode))
          (setq markdown-table-display--source-buffer source)
          (setq-local default-directory
                      (buffer-local-value 'default-directory source))
          (add-hook 'kill-buffer-hook
                    #'markdown-table-display--view-killed nil t))
        view)))

(defun markdown-table-display--restore-window-lines (window-lines)
  "Restore approximate positions recorded in WINDOW-LINES."
  (dolist (entry window-lines)
    (pcase-let ((`(,window ,point-line ,start-line) entry))
      (when (window-live-p window)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (set-window-start window (point))
          (goto-char (point-min))
          (forward-line (1- point-line))
          (set-window-point window (point)))))))

;;;###autoload
(defun markdown-table-display-refresh (&optional width)
  "Refresh the copyable table view using WIDTH or its visible width."
  (interactive)
  (let ((source (markdown-table-display--source-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (and markdown-table-display-mode
                   (not markdown-table-display--refreshing))
          (let* ((markdown-table-display--refreshing t)
                 (width (or width
                            (markdown-table-display--visible-width source)))
                 (view (markdown-table-display--ensure-view source))
                 (rendered (markdown-table-display--render-buffer-string
                            (buffer-substring-no-properties
                             (point-min) (point-max))
                            width (1- (point-min))))
                 (window-lines
                  (mapcar
                   (lambda (window)
                     (with-current-buffer view
                       (list window
                             (line-number-at-pos (window-point window))
                             (line-number-at-pos (window-start window)))))
                   (get-buffer-window-list view nil t))))
            (setq markdown-table-display--last-width width)
            (with-current-buffer view
              (markdown-table-display--preserve-link-properties)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert rendered)
                (set-buffer-modified-p nil)
                (font-lock-flush)))
            (with-current-buffer view
              (markdown-table-display--restore-window-lines window-lines))))))))

(defun markdown-table-display--show-view (source &optional window)
  "Show SOURCE's refreshed table view in WINDOW or its current windows."
  (with-current-buffer source
    (let* ((saved-source-point (point))
           (windows (if window
                        (list window)
                      (get-buffer-window-list source nil t)))
           (window-positions
            (mapcar (lambda (source-window)
                      (list source-window
                            (window-point source-window)
                            (window-start source-window)))
                    windows)))
      (setq markdown-table-display--source-point saved-source-point)
      (markdown-table-display-refresh)
      (let ((view markdown-table-display--view-buffer))
      (when (buffer-live-p view)
          (if window-positions
              (dolist (entry window-positions)
                (pcase-let ((`(,source-window ,source-point ,source-start)
                             entry))
                  (when (window-live-p source-window)
                    (let ((view-point
                           (markdown-table-display--view-position-for-source
                            view source source-point))
                          (view-start
                           (markdown-table-display--view-position-for-source
                            view source source-start)))
                      (set-window-buffer source-window view)
                      (set-window-point source-window view-point)
                      (set-window-start source-window view-start t)))))
            (with-current-buffer view
              (goto-char
               (markdown-table-display--view-position-for-source
                view source saved-source-point)))))))))

(defun markdown-table-display--show-source-in-window (window source)
  "Replace the table view in WINDOW with SOURCE at the corresponding location."
  (let* ((view-point (window-point window))
         (view-start (window-start window))
         (source-point
          (or (markdown-table-display--source-position-for-view
               source view-point)
              (buffer-local-value 'markdown-table-display--source-point source)
              (with-current-buffer source (point-min))))
         (source-start
          (markdown-table-display--source-position-for-view source view-start)))
    (set-window-buffer window source)
    (set-window-point window source-point)
    (when source-start
      (set-window-start window source-start t))))

;;;###autoload
(defun markdown-table-display-toggle ()
  "Switch between the copyable table view and its writable source."
  (interactive)
  (if (derived-mode-p 'markdown-table-display-view-mode)
      (let ((source markdown-table-display--source-buffer))
        (unless (buffer-live-p source)
          (user-error "The source buffer no longer exists"))
        (markdown-table-display--show-source-in-window
         (selected-window) source)
        (message "Editing writable Markdown source; C-c | returns to the view"))
    (unless (derived-mode-p 'markdown-mode)
      (user-error "This command is only available in Markdown buffers"))
    (if markdown-table-display-mode
        (markdown-table-display--show-view (current-buffer)
                                           (selected-window))
      (markdown-table-display-mode 1))))

(defun markdown-table-display-quit ()
  "Kill the generated table view and return its windows to the source buffer."
  (interactive)
  (unless (derived-mode-p 'markdown-table-display-view-mode)
    (user-error "This is not a Markdown table view"))
  (let ((view (current-buffer))
        (source markdown-table-display--source-buffer))
    (when (buffer-live-p source)
      (dolist (window (get-buffer-window-list view nil t))
        (markdown-table-display--show-source-in-window window source)))
    (kill-buffer view)))

(defun markdown-table-display--run-scheduled-refresh (source)
  "Refresh SOURCE if its table display mode is still active."
  (when (buffer-live-p source)
    (with-current-buffer source
      (setq markdown-table-display--refresh-timer nil)
      (when markdown-table-display-mode
        (markdown-table-display-refresh)))))

(defun markdown-table-display--schedule-refresh (&rest _ignored)
  "Schedule a view refresh after source-buffer changes."
  (when markdown-table-display--refresh-timer
    (cancel-timer markdown-table-display--refresh-timer))
  (setq markdown-table-display--refresh-timer
        (run-with-idle-timer
         markdown-table-display-refresh-delay nil
         #'markdown-table-display--run-scheduled-refresh (current-buffer))))

(defun markdown-table-display--window-size-changed (&optional _frame)
  "Refresh visible table views whose available width changed."
  (let ((sources nil))
    (dolist (window (window-list-1 nil 'nomini t))
      (let* ((buffer (window-buffer window))
             (source
              (and (buffer-live-p buffer)
                   (buffer-local-value
                    'markdown-table-display--source-buffer buffer))))
        (when (and (buffer-live-p source) (not (memq source sources)))
          (push source sources))))
    (dolist (source sources)
      (with-current-buffer source
        (let ((width (markdown-table-display--visible-width source)))
          (unless (equal width markdown-table-display--last-width)
            (markdown-table-display-refresh width)))))))

(defun markdown-table-display--cancel-timer ()
  "Cancel the current source buffer's pending view refresh."
  (when markdown-table-display--refresh-timer
    (cancel-timer markdown-table-display--refresh-timer)
    (setq markdown-table-display--refresh-timer nil)))

(defun markdown-table-display--kill-view ()
  "Kill the current source buffer's table view."
  (markdown-table-display--cancel-timer)
  (let ((source (current-buffer))
        (view markdown-table-display--view-buffer))
    (when (buffer-live-p view)
      (dolist (window (get-buffer-window-list view nil t))
        (set-window-buffer window source))
      (kill-buffer view))
    (setq markdown-table-display--view-buffer nil)))

;;;###autoload
(define-minor-mode markdown-table-display-mode
  "Show this Markdown source through a read-only, copyable table view.

The view contains real text, while this file-visiting source buffer remains
unchanged and writable.  Use `markdown-table-display-toggle' to switch between
them."
  :lighter " TblView"
  (if markdown-table-display-mode
      (progn
        (add-hook 'after-change-functions
                  #'markdown-table-display--schedule-refresh nil t)
        (add-hook 'kill-buffer-hook #'markdown-table-display--kill-view nil t)
        (markdown-table-display--show-view (current-buffer)))
    (remove-hook 'after-change-functions
                 #'markdown-table-display--schedule-refresh t)
    (remove-hook 'kill-buffer-hook #'markdown-table-display--kill-view t)
    (markdown-table-display--kill-view)
    (setq markdown-table-display--last-width nil)))

(defun markdown-table-display--maybe-enable ()
  "Enable table view unless this is the generated view buffer."
  (unless (or markdown-table-display--inhibit-auto-enable
              (derived-mode-p 'markdown-table-display-view-mode))
    (markdown-table-display-mode 1)))

(add-hook 'window-size-change-functions
          #'markdown-table-display--window-size-changed)

(provide 'markdown-table-display)
;;; markdown-table-display.el ends here
