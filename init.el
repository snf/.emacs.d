(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package emacs
  :init
  ;; Make startup faster by reducing the frequency of garbage
  ;; collection.  The default is 0.8MB.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))
  ;; Portion of heap used for allocation.  Defaults to 0.1.
  (setq gc-cons-percentage 0.6)

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file t)

  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq visible-bell t)
  (setq indent-tabs-mode -1)
  (setq split-width-threshold 260)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "")
  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'default-frame-alist
               '(font . "DejaVu Sans Mono-12"))
  (setq auto-save-file-name-transforms
        '((".*" "~/.emacs.d/auto-save-list/" t))
        backup-directory-alist
        '(("." . "~/.emacs.d/backups/")))

  (require 'tramp)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment "BASH_ENV=~/.bashrc")
  (defun my/ansi-color-apply-on-region (begin end)
    (interactive "r")
    (ansi-color-apply-on-region begin end t))
  )

;; === packages ===
(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-project-status)
              )
  :custom
  (vc-directory-exclusion-list '("node_modules" "memoizeFs_cache" "contract_graphs" ".git" "target"))
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (add-to-list 'project-switch-commands '(project-switch-to-buffer "Buffer") t)
  (add-to-list 'project-switch-commands '(treemacs-display-current-project-exclusively "Treemacs") t)

  ;; XXX_ in Emacs 29 can be updated to	https://grtcdr.tn/posts/2023-03-01.html
  ;; (setq project-vc-extra-root-markers '("Cargo.toml" ".git" ".project" "Move.toml"))
  ;; From https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
  (defcustom my/project-root-markers
    '(".git" ".project" "Move.toml") ;; Removed Cargo.toml because it was opening a new session in crates of the same workspace
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun my/project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker my/project-root-markers)
	(when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun my/project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'my/project-root-p)))
      ;; (cons 'transient (expand-file-name root)))) ;; don't expand filename to avoid saving duplicates
      (cons 'transient root)))

  ;; (defun my/project-override (dir)
  ;;   (let ((override (locate-dominating-file dir ".project")))
  ;;     (if override
  ;; 	  (cons 'transient override)
  ;; 	nil)))

  ;; Add magit-clone to projects
  ;; TODO
  ;; (advice-add 'magit-clone :around
  ;;             (lambda (magit-clone-fun &rest args)
  ;;               (apply magit-clone-fun args)
  ;;               (let ((project-dir (nth 0 args)))
  ;;                 (project-remember-project project-dir)
  ;;                 )))


  ;; (advice-add 'magit-clone-internal :around
  ;;             (lambda (magit-clone-fun &rest args)
  ;;               (apply magit-clone-fun args)
  ;;               (let ((project-dir (nth 1 args)))
  ;;                 (project-remember-project project-dir)
  ;;                 )))

  (add-to-list 'project-find-functions #'my/project-find-root)
  )


;; === deps
;; autocomplete
(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  ;; (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :bind (:map vertico-map
            ("<tab>" . vertico-insert)  ; Insert selected candidate into text area
            ;; ("<escape>" minibuffer-keyboard-quit) ; Close minibuffer
	    )
  :init
  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun my/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun my/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 my/basic-remote-try-completion my/basic-remote-all-completions nil))
  ;; (setq completion-styles '(orderless basic)
  ;; 	completion-category-overrides '((file (styles basic-remote partial-completion))))
  :init
  (vertico-mode 1)
  )

;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :after (vertico corfu)
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :after (perspective)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (advice-add #'project-find-regexp :override #'consult-ripgrep)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme consult-xref
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   consult--source-buffer
   :default nil
   :preview-key "M-.")

  ;; Perspective source
  (add-to-list 'consult-buffer-sources persp-consult-source)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode 1))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package flx)

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  ;; :straight t
  ;; :demand t
  :ensure t
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-m" . corfu-move-to-minibuffer)
              ("<escape>". corfu-quit)
              ("<return>" . nil)
              ("^M" . nil)
	      ("TAB" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . 'corfu-show-location))

  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  ;; (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)

  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 24)
  (corfu-scroll-margin 5)
  (corfu-cycle nil)

  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)

  (corfu-preview-current 'insert)       ; Preview current candidate?
  (corfu-preselect-first t)             ; Preselect first candidate?

  (corfu-on-exact-match nil)            ; Prevent automatic completion of yasnippets (and others)

  :init

  ;; (use-package corfu-history
  ;;   :hook (global-corfu-mode . corfu-history-mode))
  (use-package corfu-popupinfo
    :straight f
    :hook (global-corfu-mode . corfu-popupinfo-mode)
    :custom
    (corfu-popupinfo-delay 0.2)
    )

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

;; (use-package company)
(use-package cape
  :hook ((lsp-completion-mode . my/cape-capf-setup-lsp)
	 ;; (org-mode . my/cape-capf-setup-org)
	 )
  :init
  ;; Use cape-super-capf to merge the completion at point
  ;; https://www.reddit.com/r/emacs/comments/zs0ie5/comment/j17vrgk/?utm_source=share&utm_medium=web2x&context=3
  (defun my/cape-capf-setup-lsp ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
                       (cape-capf-buster #'lsp-completion-at-point)
                       #'cape-dabbrev
                       ;; (cape-company-to-capf #'company-yasnippet)
		       ))))

;;   (defun my/cape-capf-setup-lsp ()
;;     "Replace the default `lsp-completion-at-point' with its
;; `cape-capf-buster' version. Also add `cape-file' and
;; `company-yasnippet' backends."
;;     (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
;;           (cape-capf-buster #'lsp-completion-at-point))
;;     ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
;;     ;; listed when I want?
;;     (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Org
  ;; (defun my/cape-capf-setup-org ()
  ;;   (require 'org-roam)
  ;;   (if (org-roam-file-p)
  ;;       (org-roam--register-completion-functions-h)
  ;;     (let (result)
  ;;       (dolist (element (list
  ;;                         (cape-super-capf #'cape-ispell #'cape-dabbrev)
  ;;                         (cape-company-to-capf #'company-yasnippet))
  ;;                        result)
  ;;         (add-to-list 'completion-at-point-functions element)))
  ;;     ))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-symbol)
         ("C-c p i" . cape-ispell)))

;; utils
(use-package exec-path-from-shell
  :if (not (string= window-system 'w32))
  :init
  (exec-path-from-shell-initialize)
  )
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region)
  )
(use-package magit
  :custom
  (show-trailing-whitespace nil)
  )
(use-package git-link
  :straight (git-link :type git :host github :repo "sshaw/git-link")
  )
(use-package git-timemachine)
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )
(use-package olivetti
  :straight (olivetti :type git :host github :repo "rnkn/olivetti")
  )
(use-package rg
  :config
  (rg-define-toggle "--context 6" (kbd "C-c c"))
  (rg-define-toggle "-l --no-ignore" (kbd "C-c l"))
  :custom
  (rg-command-line-flags '("-M=160" "--max-columns-preview"))
  )
;; (use-package projectile
;;   :bind (("C-c p" . projectile-command-map))
;;   )

;; Remove trailing whitespaces in edited lines on save
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (org-mode . ws-butler-mode)
  )



(use-package eat
  :straight (
             :type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  
  )

(use-package vterm
  :if (not (string= window-system 'w32))
  :bind
  (:map vterm-mode-map
        ("M-y" . vterm-yank-pop)
	)
  (:map vterm-copy-mode-map
	("q" . vterm-copy-mode))
  :config
  (defun run-in-vterm-kill (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b)
           (kill-buffer b))))

  (defun run-in-vterm (command)
    "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
    (interactive
     (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
             (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
	(read-shell-command "Terminal command: "
                            (cons filename 0)
                            (cons 'shell-command-history 1)
                            (list filename)))))
    (with-current-buffer (vterm (concat "*" command "*"))
      (set-process-sentinel vterm--process #'run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return)))
  )

(use-package undo-fu
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )
(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory "~/.emacs.d/undo-fu-session/")
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))
  :init
  (undo-fu-session-global-mode)
  )

(use-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :commands (vundo)
  :config
  (setq vundo-compact-display t)  ;; Take less on-screen space.
  :bind ("C-x u" . vundo)
  )

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )
(use-package yasnippet-snippets)

;; langs
(use-package treesit
  :straight (:type built-in)
  :if (>= emacs-major-version 29)
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
	'(
	  (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (c . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
	  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (c_sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
	  (css . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (go . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (html . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (java . ("https://github.com/tree-sitter/tree-sitter-java"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  ;; (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
	  (make . ("https://github.com/alemuller/tree-sitter-make"))
	  (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (php . ("https://github.com/tree-sitter/tree-sitter-php"))
	  (regex . ("https://github.com/tree-sitter/tree-sitter-regex"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	  (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))
	  (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
	  (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  ;; (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
	  ))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75)))))

(use-package cov)
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo" :branch "main")
  :config
  (global-hl-todo-mode)
  )

(use-package smartparens
  :config
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "[" "]" :unless '(sp-point-before-word-p))
  (sp-local-pair '(emacs-lisp-mode rust-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode rust-ts-mode) "'" "'" :actions nil)
  :init
  (smartparens-global-mode t)
  )
(use-package conda
  :after (python-mode)
  :init
  ;; XXX_ if conda-home isn't set it throws an error
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  (add-hook 'conda-postactivate-hook (lambda () (lsp-restart-workspace)))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  )
(use-package json-mode)
;; Hangs
;; (use-package json-snatcher
;;   :custom
;;   (jsons-path-printer 'jsons-print-path-jq)
;;   )

(use-package lsp-mode
  :config
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\][123].+\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]perf.+\\'")
  :custom
  (lsp-completion-provider :none)
  (lsp-keep-workspace-alive nil)
  (lsp-use-plists t)
  (lsp-signature-render-documentation nil)
  ;; (lsp-keymap-prefix "C-c C-l")
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; :init
  ;; (setq lsp-completion-provider :none)
  )

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  )
(use-package csharp-mode
  :mode
  "\\.cs\\'"
  )
(use-package fsharp-mode
  :straight (:host github :repo "fsharp/emacs-fsharp-mode" :branch "master")
  )
;; (use-package eglot)
(use-package dockerfile-mode
  :straight (:host github :repo "spotify/dockerfile-mode" :branch "master")
  :mode
  "\\.dockerfile\\'"
  "\\dockerfile\\'"
  )

(use-package go-mode
  :hook (go-mode . lsp-deferred)
  )

(defun my-go-lint ()
  "Run golangci-lint.
It is 1 umbrella command that runs many other linters and combines their
results."
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let* ((compile-command "golangci-lint run")
         ;;(compile-command "golangci-lint run -E gosec")
         ;;(compile-command "golangci-lint run --enable-all")
         ;;(compile-command "golangci-lint run --fix")

         (default-directory (project-root buffer-file-name))
         ;; (root-obj (project-current nil))
         ;; (root-folder (if (not (null root-obj))
         ;;                  ;; extract folder field out of obj.
         ;;                  (project-root root-obj)
         ;;                ;; else get root folder manually from user
         ;;                (read-directory-name "proj root: " nil nil t)))

         ;; `compile' uses `default-directory'.
         ;; (default-directory (my-select-folder))
         )
    (call-interactively #'compile)))

(use-package flycheck-golangci-lint
  ;; Not needed separately as handled by https://github.com/nametake/golangci-lint-langserver
  :disabled t
  :hook ((go-mode go-ts-mode) . +go-setup-checkers)
  :config
  ;; (add-to-list 'flycheck-checker-max-level '(golangci-lint . warning))
  (setq flycheck-golangci-lint-enable-linters
        '("bodyclose" "depguard" "dupl" "errcheck" "exhaustive" "funlen" "gochecknoinits" "goconst"
          "gocritic" "gocognit" "gofmt" "goimports" "revive" "gosec" "gosimple" "govet" "ineffassign" "lll"
          "misspell" "noctx" "rowserrcheck" "staticcheck" "typecheck" "unparam" "unused"
          "whitespace" "gomodguard" "sqlclosecheck" "errcheck"))

  (defun +go-setup-checkers()
    (flycheck-golangci-lint-setup)
    ;; Demote golangci errors to warning
    (setq-local flycheck-local-checkers-chain '((lsp . golangci-lint)))))

(use-package groovy-mode
  :mode
  "\\.groovy\\'"
  "Jenkinsfile"
  )
(use-package hcl-mode
  :straight (:host github :repo "purcell/emacs-hcl-mode" :branch "master")
  )
(use-package markdown-mode
  :bind (:map markdown-mode-map
	      ("C-c <down>" . nil)
	      ("C-c <up>" . nil)
	      ("C-c <left>" . nil)
	      ("C-c <right>" . nil)
	      )
  )
(use-package opencl-mode
  :mode "\\.cl\\'"
  )
(use-package org
  :straight (:type built-in)
  :bind (("C-c c" . org-capture)
         :map org-mode-map
              ("C-c C-j" . consult-org-heading)
              )
  :custom
  (org-agenda-files (directory-files-recursively "/projects/notes" "\\.org$"))
  (org-refile-targets
      (list
       (cons nil '(:maxlevel . 9))
       (cons org-agenda-files '(:maxlevel . 9))
       (cons (directory-files-recursively "/projects/notes/" "\\.org$") '(:maxlevel . 9))))

   ;; '((nil :maxlevel . 9)
   ;;                      (org-agenda-files :maxlevel . 9)
   ;;                      ((directory-files-recursively "/projects/notes/" "\\.org$") :maxlevel . 9)
   ;;                    ))
  (org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (org-refile-use-outline-path t)                  ; Show full paths for refiling
  (org-reverse-note-order t)
  (org-agenda-include-diary t)
  (org-capture-templates
   '(("t" "Todo" entry (file "/projects/notes/todo.org")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("s" "Schedule todo" entry (file "/projects/notes/todo.org")
      "* TODO %?\n  SCHEDULED: <%<%Y-%m-%d %H:%M>>\n  %i\n  %a" :empty-lines 1)
     ))
  :init
  (add-hook 'org-mode-hook #'(lambda ()
			       ;; make the lines in the buffer wrap around the edges of the screen.
			       ;; to press C-c q  or fill-paragraph ever again!
                               (visual-line-mode)
                               (org-indent-mode)))
  )

(use-package org-superstar-mode
  :straight (:host github :repo "integral-dw/org-superstar-mode" :branch "master")
  :custom
  (org-superstar-remove-leading-stars t)
  :hook (org-mode . org-superstar-mode)
  )

(use-package powershell
  ;; :if (string= window-system 'w32)
  :hook (powershell-mode . lsp)
  )
(use-package python-mode
  :custom
  (py-split-window-on-execute nil)
  )
(use-package pyvenv
  :after python-mode
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))
(use-package lsp-pyright
  :after (lsp)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred
(use-package move-mode
  :straight (:host github :repo "amnn/move-mode" :branch "main")
  :config
  ;; Use shopify-cli / theme-check-language-server for Shopify's liquid syntax
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
		 '(move-mode . "move"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "move-analyzer")
      :activation-fn (lsp-activate-on "move")
      :priority -1
      :server-id 'move-analyzer))
    )
  )
;; (use-package rustic
;;   :custom
;;   (rustic-lsp-client 'lsp-mode)
;;   )
(use-package rust-mode
  :straight (:host github :repo "rust-lang/rust-mode")
  :hook
  (rust-mode . lsp)
  ;; (rust-ts-mode . lsp)
  :custom
  (rust-mode-treesitter-derive t)
  :init
  (setq rust-indent-offset 4)
  (setenv "CARGO_TARGET_DIR" "/tmp/cargo_build")
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  )

(use-package solidity-mode
  :config
  ;; Use shopify-cli / theme-check-language-server for Shopify's liquid syntax
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
		 '(solidity-mode . "solidity"))
    (lsp-register-client
     (make-lsp-client
      ;; :new-connection (lsp-stdio-connection '("solc" "--lsp"))
      :new-connection (lsp-stdio-connection '("nomicfoundation-solidity-language-server" "--stdio"))
      ;; :new-connection (lsp-stdio-connection '("vscode-solidity-server" "--stdio"))
      :activation-fn (lsp-activate-on "solidity")
      :priority -1
      :server-id 'nomic-solidity-lsp))
    )
  )

(use-package souffle-mode
  :straight (souffle-mode :type git :host github :repo "gbalats/souffle-mode")
  :custom (souffle-indent-width 2)
  :bind ("C-c C-o" . 'ff-find-other-file)
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
		 '(souffle-mode . "souffle"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "souffle-lsp")
      :activation-fn (lsp-activate-on "souffle")
      :priority -1
      :server-id 'souffle-lsp))
    )
  )
(use-package terraform-mode
  :custom
  (terraform-indent-level 4)
  )
(use-package typescript-mode
  :hook
  (typescript-mode . lsp)
  :custom
  (js-indent-level 2)
  )

(use-package prettier
  ;; :init
  ;; (global-prettier-mode)
)

(use-package web-mode
  :mode
  "\\.html?\\'"
  "\\.js\\'"
  "\\.css\\'"
  "\\.tsx\\'"
  )
(use-package impatient-mode
  :custom
  (httpd-port 9090)
  (httpd-host "0.0.0.0")
  )
(use-package yaml-mode)
(use-package yaml-pro
  :straight (yaml-pro :type git :host github :repo "zkry/yaml-pro")
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-mode)
  )
;; (use-package z3-mode
;;   :straight (z3-mode :type git :host github :repo "zv/z3-mode")
;;   )

(use-package boogie-friends
  :straight (boogie-friends :type git :host github :repo "boogie-org/boogie-friends")
  )

;; tools
;; ai
(load-file "~/.emacs.d/secret.el")
(use-package chatgpt-shell
  :straight (chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell")
  :config
  (setq chatgpt-shell-system-prompts (nconc chatgpt-shell-system-prompts '(("Chemistry" . "The user is a professional chemist with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        If you don't know the answer, say I don't know.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.")
        ("Technical Blogpost" . "Given a draft blog post targeted at developers, your task is to editorialize and improve it by following these guidelines. Before executing changes, allow room for clarifications on any doubts or questions:

1. Establish Credibility:
   - Ensure the introduction quickly establishes the author’s expertise and experience. Integrate any achievements or notable metrics but maintain a balanced tone to avoid sounding overly boastful.

2. Make the Point Quickly:
   - Apply the inverted pyramid format by placing critical information at the beginning. Ensure the first sentence addresses the who, what, when, where, and why to facilitate skimming. Review the introduction to ensure it captures the main idea swiftly and addresses why the reader should care. Verify the first three sentences clearly indicate the target audience and the benefits they’ll gain from reading.

3. Broaden Audience Appropriately:
   - Identify opportunities to expand the potential readership without diluting technical content. Simplify jargon as needed and consider whether minor adjustments could make the content accessible to a broader audience.

4. Plan the Reader’s Journey:
   - Highlight pathways for reaching the target audience effectively. Evaluate potential keywords, platforms, and strategies that could be mentioned to ensure readers have a practical understanding of how to disseminate their content.

5. Enhance Visual Engagement:
   - Suggest diagrams, screenshots, or other visual elements that complement and break up large blocks of text. Recommend cost-effective resources for creating or sourcing visuals.

6. Accommodate Skimmers:
   - Ensure the structure supports skimming by refining headings and subheadings. Check if the text flow includes bulleted lists, concise paragraphs, and key takeaways that are easy to digest at a glance.

7. General Language and Tone:
   - Maintain a conversational and approachable tone while ensuring technical precision. Simplify overly complex sentences and suggest improvements for clarity and engagement.

Before making any changes, please pause and identify any areas where clarification is needed or additional information would be beneficial.

List these clarification points, and await further instructions before continuing with the enhancements.
")
	)))

  (add-to-list 'chatgpt-shell-models
               (chatgpt-shell-openrouter-make-model
                :version "deepseek/deepseek-v3"
                :short-version "deepseek-v3"
                :label "DeepSeekV3:Free"
                :context-window 128000
                :token-width 4))

  (add-to-list 'chatgpt-shell-models
               (chatgpt-shell-openrouter-make-model
                :version "deepseek/deepseek-r1-0528"
                :short-version "deepseek-r1-0528"
                :label "DeepSeekR1:Free"
                :context-window 128000
                :token-width 4))
  (add-to-list 'chatgpt-shell-models
               (chatgpt-shell-openrouter-make-model
                :version "google/gemini-2.5-pro"
                :short-version "google/gemini-2.5-pro"
                :label "Gemini2.5 Pro Preview"
                :context-window 128000
                :token-width 4))

  ;; (setq chatgpt-shell-models ())

  ;; (advice-add 'chatgpt-shell-openrouter-models :filter-return
  ;;             #'chatgpt-shell-openrouter-add-models)

  ;; (defun chatgpt-shell-openrouter-add-models (models)
  ;;   (append models
  ;;           (list (chatgpt-shell-openrouter-make-model
  ;;                  :version "deepseek/deepseek-r1:free"
  ;;                  :short-version "deepseek-r1:free"
  ;;                  :label "DeepSeekR1:Free"
  ;;                  :context-window 128000
  ;;                  ))))

  ;; (advice-add 'chatgpt-shell-openrouter-models :filter-return
  ;;             #'chatgpt-shell-openrouter-add-models)
  )

(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :bind
  ("C-c g" . gptel-menu)
  :custom
  (gptel-api-key chatgpt-shell-openai-key)
  ;; (gptel-log-level "debug")
  :config
  (gptel-make-openai "OpenRouter"               ;Any name you want
                     :host "openrouter.ai"
                     :endpoint "/api/v1/chat/completions"
                     :stream t
                     :key 'chatgpt-shell-openrouter-key ;can be a function that returns the key
                     :models '(openai/gpt-3.5-turbo
                               openai/o4-mini-high ;; reasoning set to high
                               openai/gpt-4o-mini-search-preview
                               openai/gpt-4o-search-preview
                               qwen/qwq-32b
                               qwen/qwen3-235b-a22b:free
                               perplexity/sonar
                               perplexity/sonar-reasoning-pro
                               perplexity/sonar-deep-research
                               microsoft/mai-ds-r1:free
                               anthropic/claude-4-sonnet
                               anthropic/claude-4-sonnet:thinking
                               deepseek/deepseek-r1-0528
                               google/gemini-2.5-flash:thinking
                               google/gemini-2.5-pro))

  (gptel-make-openai "OpenRouter-Thinking"               ;Any name you want
                     :host "openrouter.ai"
                     :endpoint "/api/v1/chat/completions"
                     :stream t
                     :key 'chatgpt-shell-openrouter-key ;can be a function that returns the key
                     :request-params '(:reasoning_tokens 5000)
                     :models '(
                               google/gemini-2.5-flash
                     ))

  (add-to-list 'gptel-directives
               '(quant . "You are a large language model and a helpful quantitative researcher specializing in trading of blockchain products. Provide good answers as the company depends on you. Be concise and only expand them if the user asks. You are also good at coding in python but only provide code if the user asks."))
)

(use-package mcp
  :straight (mcp :type git :host github :repo "lizqwerscott/mcp.el")
  ;; :custom
  ;; (mcp-log-level 'debug)
  :config
  (setq mcp-hub-servers
      '(
        ;; ("google-search" . (
        ;;                 :command "node"
        ;;                 :args ("/opt/google-search-mcp/dist/google-search.js")
        ;;                 :env (:GOOGLE_API_KEY google-search-key
        ;;                       :GOOGLE_SEARCH_ENGINE_ID "your-custom-search-engine-id"
        ;;                       )))
        ;; ("docs-rs" . (
        ;;               :command "/opt/docs-rs-mcp/target/release/docs-rs-mcp"
        ;;                          ))
        ("task-master-ai" . (
                             :command "npx"
                             :args ("-y" "--package=task-master-ai" "task-master-ai")
                             :env (:OPENROUTER_API_KEY chatgpt-shell-openrouter-key)
                             ))

        ))

  (defun gptel-mcp-register-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (apply #'gptel-make-tool
                         tool))
              tools)))

  (defun gptel-mcp-use-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (let ((path (list (plist-get tool :category)
                                    (plist-get tool :name))))
                    (push (gptel-get-tool path)
                          gptel-tools)))
              tools)))

  (defun gptel-mcp-close-use-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (let ((path (list (plist-get tool :category)
                                    (plist-get tool :name))))
                    (setq gptel-tools
                          (cl-remove-if #'(lambda (tool)
                                            (equal path
                                                   (list (gptel-tool-category tool)
                                                         (gptel-tool-name tool))))
                                        gptel-tools))))
              tools)))

)


;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :config
;;   ;; Use claude-3-5-sonnet cause it is best in aider benchmark
;;   ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
;;   ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   ;; Or use chatgpt model since it is most well known
;;   (setq aider-args '("--model" "openrouter/gemini/gemini-2.5-pro-preview-06-05" "--thinking-tokens" "32k" "--no-gitignore" "--edit-format" "diff-fenced"))
;;   (setenv "OPENAI_API_KEY" chatgpt-shell-openai-key)
;;   (setenv "OPENROUTER_API_KEY" chatgpt-shell-openrouter-key)
;;   ;; Or use gemini v2 model since it is very good and free
;;   ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
;;   ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-show-diff-after-change nil)
  :config
  ;; (setq aidermacs-program '("uvx python@3.12 -m aider"))
  ;; (setq aidermacs-extra-args '("--architect" "--model" "openrouter/deepseek/deepseek-r1" "--editor-model" "openrouter/anthropic/claude-3.7-sonnet" "--no-gitignore"))
  (setq aidermacs-extra-args '("--model" "openrouter/google/gemini-2.5-pro" "--thinking-tokens" "32k" "--no-gitignore" "--edit-format" "diff-fenced"))
  ;; (setq aidermacs-extra-args '("--model" "o3" "--reasoning-effort" "high"))
  ;; (setq aidermacs-extra-args '("--model" "openrouter/moonshotai/kimi-k2"))
  ;; (setq aidermacs-extra-args '("--model" "openrouter/z-ai/glm-4.5"))
  ;; (setq aidermacs-extra-args '("--architect" "--model" "openrouter/deepseek/deepseek-r1-0528" "--editor-model" "--model" "openrouter/z-ai/glm-4.5"))
  ;; (setq aidermacs-extra-args '("--model" "openrouter/qwen/qwen3-coder"))
  ;;  --line-endings crlf
  (setenv "OPENAI_API_KEY" chatgpt-shell-openai-key)
  (setenv "OPENROUTER_API_KEY" chatgpt-shell-openrouter-key)
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :custom
  (claude-code-ide-cli-path "/usr/local/bin/ccr-wrapper")
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  )

(use-package whisper
  :straight (:host github :repo "natrys/whisper.el" :files ("*.el"))
  :bind ("C-c w" . whisper-run)
  :config
  (setq whisper-install-directory "/tmp/"
        whisper--ffmpeg-input-format "alsa"
        whisper--ffmpeg-input-device "hw:1"
        ;; whisper-install-directory "~/.emacs.d/whisper/"
        whisper-model "base"
        ;; whisper-model "medium"
        ;; whisper-language "en"
        whisper-language "es"
        ;; whisper-translate nil
        whisper-translate t
        whisper-use-threads (/ (num-processors) 4))

  (defun my/whisper-display-in-buffer (text)
    "Display Whisper transcription in a dedicated buffer."
    (let ((buf (get-buffer-create "*Whisper Transcription*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert text)
        (view-mode 1))
      (display-buffer buf)))

  (defun my/whisper-record-to-buffer ()
    "Start background recording with whisper.el and show result in buffer."
    (interactive)
    (let ((orig-hook whisper-after-transcription-hook))
      (add-hook 'whisper-after-transcription-hook
                (lambda ()
                  (when-let (text (buffer-string))
                    (my/whisper-display-in-buffer text)
                    (remove-hook 'whisper-after-transcription-hook this-hook))))
      (whisper-run))))

(use-package copilot
  ;; :after company-mode
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :bind (:map copilot-mode-map
              ("C-p" . copilot-accept-completion)
              ("M-<right>" . copilot-next-completion)
              ("M-<left>" . copilot-previous-completion)
              ;; ("s-w" . copilot-accept-completion-by-word)
              ;; ("s-l" . copilot-accept-completion-by-line)
              )
  :hook
  (rust-mode . copilot-mode)
  ;; (rust-ts-mode . copilot-mode)
  ;; :config
  ;; (defun company-copilot-tab ()
  ;;   (interactive)
  ;;   (or (copilot-accept-completion)
  ;;       (company-indent-or-complete-common nil)))

  ;;       				; modify company-mode behaviors
  ;; (with-eval-after-load 'company
  ;;       				; disable inline previews
  ;;   (delq 'company-preview-if-just-one-frontend company-frontends)
  ;;       				; enable tab completion
  ;;   (define-key company-mode-map (kbd "<tab>") 'company-copilot-tab)
  ;;   (define-key company-mode-map (kbd "TAB") 'company-copilot-tab)
  ;;   (define-key company-active-map (kbd "<tab>") 'company-copilot-tab)
  ;;   (define-key company-active-map (kbd "TAB") 'company-copilot-tab))
  )

(use-package minuet
  ;; :after company-mode
  :straight (:host github :repo "milanglacier/minuet-ai.el")
  ;; :disabled
  :bind
  (
   ;; ("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-<left>" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-<right>" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("C-p" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ;; ("M-a" . #'minuet-accept-suggestion-line)
   ;; ("M-e" . #'minuet-dismiss-suggestion)
   )
  ;; :hook
  ;; (rust-mode . copilot-mode)
  ;; (rust-ts-mode . copilot-mode)
  ;; :config
  ;; (defun company-copilot-tab ()
  ;;   (interactive)
  ;;   (or (copilot-accept-completion)
  ;;       (company-indent-or-complete-common nil)))
  :config
  (setq minuet-provider 'openai-compatible)
  (setq minuet-request-timeout 2.5)
  (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
  (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

  (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key 'chatgpt-shell-openrouter-key)
  ;; (plist-put minuet-openai-compatible-options :model "deepseek/deepseek-chat-v3-0324")
  ;; (plist-put minuet-openai-compatible-options :model "mistralai/codestral-2501")
  ;; (plist-put minuet-openai-compatible-options :model "google/gemini-2.5-flash-preview")
  ;; (plist-put minuet-openai-compatible-options :model "inception/mercury-coder-small-beta")
  (plist-put minuet-openai-compatible-options :model "mistralai/codestral-2508")
  ;; (plist-put minuet-openai-compatible-options :model "qwen/qwen3-coder")

  ;; Prioritize throughput for faster completion
  (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  )

;; notifications
(use-package alert
  :config
  (run-with-timer 0 (* 30 60) #'alert "30 minutes have passed!" :style 'fringe)
  )
;; ui
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
)
(use-package anzu ;; shows number of matches in isearch-mode
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (progn
    (global-anzu-mode +1)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
    ))
(use-package avy
  :bind (
	 ("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-word-1))
  )
(use-package beacon
  :init (beacon-mode 1)
  :config
  (add-hook 'shell-mode-hook (lambda() (setq-local beacon-mode nil)))
  )
(use-package dtrt-indent
  :straight (dtrt-indent :type git :host github :repo "jscheid/dtrt-indent")
  :init
  (dtrt-indent-global-mode)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  )
(use-package idle-highlight-mode
  :custom
  (idle-highligh-ignore-modes (list 'org-mode))
  :config (global-idle-highlight-mode)
  )
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode)
  )
(use-package treemacs
  :bind ("C-x p t" . treemacs-display-current-project-exclusively)
  )

;; (use-package which-key
;;   :init
;;   (which-key-mode)
;;   ;; XXX This doesn't work, couldn't find where to hook it in which-key
;;   :bind (:map which-key-mode-map
;; 	      ("C-h" . embark-prefix-help-command))
;;   )

;;=====

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Don't kill-word in minibuffer
(define-key minibuffer-local-map [M-backspace] 'backward-delete-word)
(define-key minibuffer-local-map [C-backspace] 'backward-delete-word)

;; (windmove-default-keybindings 'meta)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; Disable annoying keys
(global-unset-key (kbd "C-z"))

;; Find file at point
(global-set-key (kbd "C-c C-o") 'ffap)

(defun display-number-at-point ()
  (interactive)
  (let*
      ((symbol (thing-at-point 'symbol))
       (base (cond
	      ((string-match-p "^-?0x[0-9a-fA-F]+$" symbol)
	       '16)
	      ((string-match-p "^-?[0-9]+$" symbol)
	       '10)
	      (t nil)
	      ))
       (strnum (if (eq base 16)
		   (save-match-data
		     (when (string-match "^-?0x\\([0-9a-fA-F]+$\\)" symbol)
		       (match-string 1 symbol)))
		 (progn symbol)
		 )
	       )
       (num (string-to-number strnum base))
       )

    (message (format "Hex: 0x%x  Dec: %d  Oct: 0%o" num num num))
    ))

(global-set-key (kbd "C-c s") 'display-number-at-point)

;; Don't accumulate too many dired buffers
(setq dired-kill-when-opening-new-dired-buffer t)
;; v this is supposed to change the behaviour of find-name-dired but didn't work so added find-iname-dired
;; (defvar read-file-name-completion-ignore-case)
(defun find-iname-dired (dir pattern)
  "Search DIR recursively for files matching the globbing PATTERN,
and run Dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The default command run (after changing into DIR) is

    find . -name \\='PATTERN\\=' -ls

See `find-name-arg' to customize the arguments."
  (interactive
   "DFind-name (directory): \nsFind-name (filename wildcard): ")
  (find-dired dir (concat  "-iname " (shell-quote-argument pattern))))

;; (defun my-resize-margins ()
;;   (let ((margin-size (/ (- (frame-width) 80) 2)))
;;     (set-window-margins nil margin-size margin-size)))

;; (add-hook 'window-configuration-change-hook #'my-resize-margins)
;; (my-resize-margins)

;; iSearch
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(delete-selection-mode)

;; (defun my/reuse-error-buffer ()
;;   "Ensure errors are shown in the same buffer without jumping to it."
;;   (let ((win (get-buffer-window (next-error-find-buffer t))))
;;     (when win
;;       (with-selected-window win
;;         (goto-char (point))))))

;; (defun my/reuse-error-buffer ()
;;   "Ensure errors are displayed in the same buffer without selecting or jumping."
;;   (let ((buffer (next-error-find-buffer t)))
;;     (when buffer
;;       (with-current-buffer buffer
;;         (goto-char (point))))))

;; (defun my/reuse-error-buffer ()
;;   "Ensure errors are shown in the same buffer."
;;   (let ((win (display-buffer (next-error-find-buffer t))))
;;     (when win
;;       (select-window win))))

;; (defun previous-error-no-select (&optional arg)
;;   "Move to previous error and show it in reused buffer without selecting."
;;   (interactive "p")
;;   (previous-error arg)
;;   (my/reuse-error-buffer))

;; (defun next-error-no-select (&optional arg)
;;   "Move to next error and show it in reused buffer without selecting."
;;   (interactive "p")
;;   (next-error arg)
;;   (my/reuse-error-buffer))

;; Apparently Emacs moves the file to back it up and then copies it back to the original, this changes that
(setq backup-by-copying t)

;; Performance
;; Make gc pauses faster by decreasing the threshold.
;; Optimize the GC
(use-package gcmh
  :straight (gcmh :type git :host gitlab :repo "koral/gcmh")
  :init
  (gcmh-mode 1)
  :custom
  (gcmh-verbose nil)
  (gcmh-idle-delay 1)
  )
;; Previous GC hacks
;; (setq gc-cons-threshold (* 2 1000 1000))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(put 'list-timers 'disabled nil)
(put 'upcase-region 'disabled nil)
