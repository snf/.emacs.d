;;; markdown-table-display-tests.el --- Tests for table display -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-table-display)

(ert-deftest markdown-table-display-render-fits-and-hides-markup ()
  (let* ((source (concat
                  "| Name | Details |\n"
                  "|---|---|\n"
                  "| **Alpha** | [read this](https://example.com/a/very/long/url) and `code` |"))
         (rendered-rows (markdown-table-display--render-rows source 34))
         (rendered (string-join rendered-rows "\n")))
    (should (string-match-p "Alpha" rendered))
    (should (string-match-p "read this" rendered))
    (should (string-match-p "code" rendered))
    (should-not (string-match-p "https://" rendered))
    (should-not (string-match-p "\\*\\*" rendered))
    (let ((alpha (string-match "Alpha" rendered))
          (link (string-match "read this" rendered))
          (code (string-match "code" rendered)))
      (should (memq 'markdown-bold-face
                    (ensure-list (get-text-property
                                  alpha 'font-lock-face rendered))))
      (should (equal (get-text-property link 'markdown-table-url rendered)
                     "https://example.com/a/very/long/url"))
      (should (eq (lookup-key (get-text-property link 'keymap rendered)
                              (kbd "RET"))
                  #'markdown-table-display-open-link))
      (should (eq (lookup-key (get-text-property link 'keymap rendered)
                              [follow-link])
                  #'markdown-table-display-open-link))
      (should (memq 'markdown-inline-code-face
                    (ensure-list (get-text-property
                                  code 'font-lock-face rendered)))))
    (dolist (line (split-string rendered "\n"))
      (should (<= (string-width line) 34)))))

(ert-deftest markdown-table-display-view-is-real-read-only-text ()
  (with-temp-buffer
    (insert (concat
             "Before\n\n"
             "| A | Long text |\n"
             "|---|---|\n"
             "| **strong** | words that *need* [wrapping](https://example.com) in a narrow window |\n\n"
             "```\n"
             "| Not | A table |\n"
             "|---|---|\n"
             "```\n"))
    (let ((source-buffer (current-buffer))
          (source-text (buffer-string)))
      (markdown-table-display-mode 1)
      (markdown-table-display-refresh 28)
      (should (equal (buffer-string) source-text))
      (should (buffer-live-p markdown-table-display--view-buffer))
      (with-current-buffer markdown-table-display--view-buffer
        (should buffer-read-only)
        (should (string-match-p "wrapping" (buffer-string)))
        (should (string-match-p "narrow window" (buffer-string)))
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "strong")
        (should (memq 'markdown-bold-face
                      (ensure-list (get-text-property
                                    (1- (point)) 'font-lock-face))))
        (search-forward "need")
        (should (memq 'markdown-italic-face
                      (ensure-list (get-text-property
                                    (1- (point)) 'font-lock-face))))
        (search-forward "wrapping")
        (should (equal (get-text-property (1- (point))
                                          'markdown-table-url)
                       "https://example.com"))
        (should (eq (lookup-key (get-text-property (1- (point)) 'keymap)
                                (kbd "RET"))
                    #'markdown-table-display-open-link))
        ;; Fenced examples remain untouched rather than being table-rendered.
        (should (string-match-p "| Not | A table |" (buffer-string)))
        ;; Copying reads generated characters, not an overlay display spec.
        (kill-new (buffer-substring-no-properties (point-min) (point-max)))
        (should (equal (current-kill 0) (buffer-string))))
      (with-current-buffer source-buffer
        (markdown-table-display-mode -1)
        (should (equal (buffer-string) source-text))
        (should-not (buffer-live-p markdown-table-display--view-buffer))))))

(ert-deftest markdown-table-display-link-command-opens-stored-url ()
  (with-temp-buffer
    (insert (markdown-table-display--link-text
             "example" "https://example.com/path"))
    (goto-char (point-min))
    (let (opened)
      (cl-letf (((symbol-function 'markdown--browse-url)
                 (lambda (url) (setq opened url))))
        (markdown-table-display-open-link))
      (should (equal opened "https://example.com/path")))))

(ert-deftest markdown-table-display-empty-code-does-not-span-table-rows ()
  (let* ((source (concat
                  "| Hash | Delta | Note |\n"
                  "|---|---|---|\n"
                  "| [tx](https://example.com/tx) | price_reference_raw `123` -> `` | done |\n"
                  "| next | **bold** | final |\n"))
         (rendered (markdown-table-display--render-buffer-string source 44)))
    (should (string-match-p "price_reference_raw" rendered))
    (should (string-match-p "bold" rendered))
    (dolist (line (split-string rendered "\n" t))
      (should (<= (string-width line) 44)))))

(ert-deftest markdown-table-display-detects-table-without-outer-pipes ()
  (let ((rendered (markdown-table-display--render-buffer-string
                   "Name | Details\n---|---\nAlpha | several words to wrap\n"
                   24)))
    (should (string-match-p "several" rendered))
    (dolist (line (split-string rendered "\n" t))
      (should (<= (string-width line) 24)))))

(ert-deftest markdown-table-display-toggle-switches-view-and-source ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *table-display-source*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) source)
            (with-current-buffer source
              (insert "| A | Detail |\n|---|---|\n| 1 | copy these words |\n")
              (markdown-mode)
              (markdown-table-display-mode 1))
            (let ((view (window-buffer (selected-window))))
              (should (not (eq view source)))
              (with-current-buffer view
                (should (derived-mode-p 'markdown-table-display-view-mode))
                (goto-char (point-min))
                (search-forward "copy")
                (markdown-table-display-toggle))
              (should (eq (window-buffer (selected-window)) source))
              (with-current-buffer source
                (should-not buffer-read-only))))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest markdown-table-display-toggle-preserves-location-and-q-closes-view ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *table-display-location-source*"))
          view
          target-line
          start-line)
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) source)
            (with-current-buffer source
              (dotimes (number 18)
                (insert (format "Prelude line %02d\n" number)))
              (insert (concat
                       "| Name | Detail |\n"
                       "|---|---|\n"
                       "| target | words kept near point |\n"))
              (dotimes (number 30)
                (insert (format "Following line %02d\n" number)))
              (markdown-mode)
              (markdown-table-display-mode 1))
            ;; Return to the source, choose a point and scroll position, then
            ;; enter the view again as a user would with C-c |.
            (with-current-buffer (window-buffer (selected-window))
              (markdown-table-display-toggle))
            (with-current-buffer source
              (goto-char (point-min))
              (forward-line 12)
              (setq start-line (line-beginning-position))
              (search-forward "target")
              (setq target-line (line-beginning-position))
              (set-window-point (selected-window) (point))
              (set-window-start (selected-window) start-line t)
              (markdown-table-display-toggle))
            (setq view (window-buffer (selected-window)))
            (with-current-buffer view
              (should (derived-mode-p 'markdown-table-display-view-mode))
              (should (= (markdown-table-display--property-at
                          (window-point (selected-window))
                          'markdown-table-source-position)
                         target-line))
              (should (= (markdown-table-display--property-at
                          (window-start (selected-window))
                          'markdown-table-source-position)
                         start-line))
              (should (eq (lookup-key markdown-table-display-view-mode-map
                                      (kbd "q"))
                          #'markdown-table-display-quit))
              (markdown-table-display-quit))
            (should-not (buffer-live-p view))
            (should (eq (window-buffer (selected-window)) source)))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest markdown-table-display-revert-reloads-source-and-view ()
  (let ((file (make-temp-file "markdown-table-display-" nil ".md"))
        source
        view)
    (unwind-protect
        (progn
          (write-region
           "| Name | Detail |\n|---|---|\n| Before | old text |\n"
           nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (markdown-mode)
            (markdown-table-display-mode 1)
            (markdown-table-display-refresh 40)
            (setq view markdown-table-display--view-buffer))
          (write-region
           "| Name | Detail |\n|---|---|\n| **After** | [new text](https://example.com) |\n"
           nil file nil 'silent)
          (with-current-buffer view
            (should (eq (lookup-key markdown-table-display-view-mode-map
                                    (kbd "r"))
                        #'revert-buffer))
            (revert-buffer t t)
            (should (string-match-p "After" (buffer-string)))
            (should (string-match-p "new text" (buffer-string)))
            (should-not (string-match-p "Before" (buffer-string))))
          (with-current-buffer source
            (should (string-match-p "After" (buffer-string)))
            (should-not (string-match-p "Before" (buffer-string)))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (set-buffer-modified-p nil))
        (kill-buffer source))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'markdown-table-display-tests)
;;; markdown-table-display-tests.el ends here
