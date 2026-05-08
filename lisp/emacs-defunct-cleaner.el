;;; emacs-defunct-cleaner.el --- Reap defunct Emacs children -*- lexical-binding: t; -*-

;;; Commentary:
;; Small wrapper around a local Emacs dynamic module that exposes waitpid(2).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emacs-defunct-cleaner nil
  "Reap defunct child processes from Emacs."
  :group 'tools)

(defcustom emacs-defunct-cleaner-interval 1800
  "Seconds between automatic defunct child reap runs."
  :type 'number)

(defcustom emacs-defunct-cleaner-initial-delay 60
  "Seconds to wait before the first automatic reap run."
  :type 'number)

(defcustom emacs-defunct-cleaner-max-reap-per-run 2000
  "Maximum number of children to reap in one run."
  :type 'integer)

(defcustom emacs-defunct-cleaner-verbose nil
  "When non-nil, log successful automatic reap runs."
  :type 'boolean)

(defcustom emacs-defunct-cleaner-c-compiler "gcc"
  "C compiler used to build the waitpid module."
  :type 'string)

(defcustom emacs-defunct-cleaner-source-file
  (expand-file-name "emacs-waitpid-module.c"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "C source file for the waitpid dynamic module."
  :type 'file)

(defcustom emacs-defunct-cleaner-module-file
  (expand-file-name
   (concat "emacs-waitpid-module" (or module-file-suffix ".so"))
   (file-name-directory (or load-file-name buffer-file-name)))
  "Compiled waitpid dynamic module file."
  :type 'file)

(defvar emacs-defunct-cleaner--timer nil)
(defvar emacs-defunct-cleaner--loaded-module-file nil)

(declare-function emacs-waitpid "emacs-waitpid-module" (pid options))
(declare-function emacs-waitpid-reap-loop "emacs-waitpid-module" (&optional limit))

(defun emacs-defunct-cleaner--module-supported-p ()
  "Return non-nil when this Emacs can load dynamic modules."
  (and (boundp 'module-file-suffix)
       module-file-suffix
       (fboundp 'module-load)))

(defun emacs-defunct-cleaner--source-dir ()
  "Return the directory containing the cleaner source files."
  (file-name-directory (or load-file-name buffer-file-name
                           emacs-defunct-cleaner-source-file)))

(defun emacs-defunct-cleaner--include-dirs ()
  "Return candidate include directories for emacs-module.h."
  (delq nil
        (list (getenv "EMACS_MODULE_INCLUDE_DIR")
              (when invocation-directory
                (expand-file-name "../include" invocation-directory))
              (when invocation-directory
                (expand-file-name "../../include" invocation-directory))
              "/usr/local/include"
              "/usr/include"
              "/opt/homebrew/include")))

(defun emacs-defunct-cleaner--include-dir ()
  "Return a directory containing emacs-module.h, or nil."
  (cl-find-if (lambda (dir)
                (and dir
                     (file-readable-p
                      (expand-file-name "emacs-module.h" dir))))
              (emacs-defunct-cleaner--include-dirs)))

(defun emacs-defunct-cleaner--stale-p ()
  "Return non-nil when the module should be rebuilt."
  (or (not (file-exists-p emacs-defunct-cleaner-module-file))
      (file-newer-than-file-p emacs-defunct-cleaner-source-file
                              emacs-defunct-cleaner-module-file)))

(defun emacs-defunct-cleaner--compile-args (include-dir output-file)
  "Return compiler arguments using INCLUDE-DIR and OUTPUT-FILE."
  (append
   (if (eq system-type 'darwin)
       '("-dynamiclib" "-fPIC")
     '("-shared" "-fPIC"))
   '("-Wall" "-Wextra" "-O2")
   (list (concat "-I" include-dir)
         "-o" output-file
         emacs-defunct-cleaner-source-file)))

;;;###autoload
(defun emacs-defunct-cleaner-compile (&optional force)
  "Compile the waitpid module when stale.
With FORCE, compile even if the existing module is newer than the source."
  (interactive "P")
  (unless (emacs-defunct-cleaner--module-supported-p)
    (user-error "This Emacs was not built with dynamic module support"))
  (unless (file-readable-p emacs-defunct-cleaner-source-file)
    (user-error "Missing waitpid module source: %s"
                emacs-defunct-cleaner-source-file))
  (if (not (or force (emacs-defunct-cleaner--stale-p)))
      emacs-defunct-cleaner-module-file
    (let ((compiler (or (executable-find emacs-defunct-cleaner-c-compiler)
                        (executable-find "cc")))
          (include-dir (emacs-defunct-cleaner--include-dir)))
      (unless compiler
        (user-error "Could not find C compiler `%s' or `cc'"
                    emacs-defunct-cleaner-c-compiler))
      (unless include-dir
        (user-error "Could not find emacs-module.h; set EMACS_MODULE_INCLUDE_DIR"))
      (make-directory (file-name-directory emacs-defunct-cleaner-module-file) t)
      (let* ((temporary-file-directory
              (file-name-directory emacs-defunct-cleaner-module-file))
             (tmp-file (make-temp-file "emacs-waitpid-module-" nil
                                       module-file-suffix))
             (args (emacs-defunct-cleaner--compile-args include-dir tmp-file))
             (buffer (get-buffer-create "*emacs-defunct-cleaner-compile*"))
             (status nil))
        (unwind-protect
            (with-current-buffer buffer
              (erase-buffer)
              (setq status (apply #'call-process compiler nil buffer nil args))
              (if (eq status 0)
                  (progn
                  (rename-file tmp-file emacs-defunct-cleaner-module-file t)
                  (set-file-modes emacs-defunct-cleaner-module-file #o755)
                  (when (called-interactively-p 'interactive)
                    (message "Compiled %s" emacs-defunct-cleaner-module-file))
                  emacs-defunct-cleaner-module-file)
                (user-error "Failed to compile waitpid module; see %s"
                            (buffer-name buffer))))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file)))))))

;;;###autoload
(defun emacs-defunct-cleaner-load (&optional force-compile)
  "Compile if needed and load the waitpid module.
With FORCE-COMPILE, rebuild the module before loading it."
  (interactive "P")
  (let ((module-file (emacs-defunct-cleaner-compile force-compile)))
    (if (featurep 'emacs-waitpid-module)
        (when (and (emacs-defunct-cleaner--stale-p)
                   (not (equal module-file
                               emacs-defunct-cleaner--loaded-module-file)))
          (message "waitpid module was already loaded; restart Emacs to use the rebuilt module"))
      (module-load module-file)
      (setq emacs-defunct-cleaner--loaded-module-file module-file))
    (unless (fboundp 'emacs-waitpid-reap-loop)
      (user-error "waitpid module loaded but did not define emacs-waitpid-reap-loop"))
    module-file))

;;;###autoload
(defun emacs-defunct-cleaner-reap-now (&optional limit)
  "Reap defunct child processes now.
Optional LIMIT overrides `emacs-defunct-cleaner-max-reap-per-run'."
  (interactive "P")
  (emacs-defunct-cleaner-load)
  (let* ((max-reap (if limit
                       (prefix-numeric-value limit)
                     emacs-defunct-cleaner-max-reap-per-run))
         (count (emacs-waitpid-reap-loop max-reap)))
    (when (or emacs-defunct-cleaner-verbose
              (called-interactively-p 'interactive))
      (message "Reaped %d defunct Emacs child process%s"
               count
               (if (= count 1) "" "es")))
    count))

(defun emacs-defunct-cleaner--timer-run ()
  "Run one automatic defunct child reap pass."
  (condition-case err
      (let ((count (emacs-defunct-cleaner-reap-now)))
        (when (and emacs-defunct-cleaner-verbose (> count 0))
          (message "Reaped %d defunct Emacs child process%s"
                   count
                   (if (= count 1) "" "es"))))
    (error
     (message "emacs-defunct-cleaner failed: %s" (error-message-string err)))))

;;;###autoload
(define-minor-mode emacs-defunct-cleaner-mode
  "Periodically reap defunct child processes from this Emacs."
  :global t
  :group 'emacs-defunct-cleaner
  (if emacs-defunct-cleaner-mode
      (progn
        (emacs-defunct-cleaner-load)
        (setq emacs-defunct-cleaner--timer
              (run-at-time emacs-defunct-cleaner-initial-delay
                           emacs-defunct-cleaner-interval
                           #'emacs-defunct-cleaner--timer-run)))
    (when emacs-defunct-cleaner--timer
      (cancel-timer emacs-defunct-cleaner--timer)
      (setq emacs-defunct-cleaner--timer nil))))

(provide 'emacs-defunct-cleaner)

;;; emacs-defunct-cleaner.el ends here
