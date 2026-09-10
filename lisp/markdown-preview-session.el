;;; markdown-preview-session.el --- Reliable local WebKit preview -*- lexical-binding: t; -*-

;;; Commentary:
;; grip-mode opens the browser before go-grip listens, and leaves the mode
;; enabled when the browser is closed.  Separate server startup, HTTP readiness,
;; and display; retain the widget on its source buffer, not in WebKit's global
;; last-session variable.  Keep this integration outside the package checkout.

;;; Code:
(require 'cl-lib)
(require 'grip-mode)
(require 'url-http)
(require 'url-util)

(defvar markdown-preview-session-timeout 5
  "Maximum seconds to wait for the preview server to respond successfully.")
(defvar-local markdown-preview-session--widget nil
  "WebKit widget belonging to this Markdown source buffer.")

(declare-function markdown-github-preview--enable-autoscroll "init" (&optional session))
(declare-function xwidget-live-p "xwidget.c" (xwidget))
(declare-function xwidget-buffer "xwidget.c" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-webkit-new-session "xwidget" (url))
(declare-function xwidget-webkit-current-session "xwidget" ())

(defun markdown-preview-session--free-port ()
  "Ask the OS for a currently available loopback port.
Release it before go-grip binds; a remaining bind race is reported on startup."
  (let ((listener (make-network-process
                   :name "markdown-preview-port" :server t
                   :host "127.0.0.1" :family 'ipv4 :service t :noquery t)))
    (unwind-protect (process-contact listener :service)
      (delete-process listener))))

(defun markdown-preview-session--http-ready-p (url)
  "Return non-nil if URL responds with HTTP 200 within a short timeout."
  (let ((url-proxy-services nil)
        (url-show-status nil)
        (url-request-method "GET")
        (url-request-extra-headers '(("Connection" . "close"))))
    (let ((response (condition-case nil
                        (url-retrieve-synchronously url t t 0.3)
                      (error nil))))
      (when (buffer-live-p response)
        (unwind-protect
            (with-current-buffer response (eq url-http-response-status 200))
          (kill-buffer response))))))

(defun markdown-preview-session--failure (reason)
  "Report REASON and retain the server log for inspection."
  (let ((log (and (processp grip--process) (process-buffer grip--process))))
    (user-error "%s%s" reason
                (if (buffer-live-p log)
                    (format "; see buffer %s" (buffer-name log)) ""))))

(defun markdown-preview-session--wait (url)
  "Wait a bounded time for URL, detecting a server exit before opening WebKit."
  (let ((deadline (+ (float-time) markdown-preview-session-timeout))
        ready)
    (while (and (not ready) (< (float-time) deadline))
      (unless (process-live-p grip--process)
        (markdown-preview-session--failure "Markdown preview server exited"))
      (setq ready (markdown-preview-session--http-ready-p url))
      (unless ready (accept-process-output grip--process 0.05)))
    (unless ready
      (markdown-preview-session--failure "Timed out waiting for Markdown preview"))))

(defun markdown-preview-session--display (url)
  "Display URL in the source's own widget, recreating it if it was closed."
  (require 'xwidget)
  (let ((source (current-buffer))
        (widget markdown-preview-session--widget))
    (if (and widget (xwidget-live-p widget)
             (buffer-live-p (xwidget-buffer widget)))
        (progn
          (pop-to-buffer (xwidget-buffer widget))
          ;; Retry navigation even if this widget previously showed an error.
          (xwidget-webkit-goto-uri widget url))
      (xwidget-webkit-new-session url)
      (setq widget (xwidget-webkit-current-session))
      (with-current-buffer source
        (setq markdown-preview-session--widget widget)))
    (markdown-github-preview--enable-autoscroll widget)))

(defun markdown-preview-session-open ()
  "Ensure the current Markdown source has a ready server and visible preview."
  (unless (and (display-graphic-p) (featurep 'xwidget-internal))
    (user-error "Markdown preview requires a graphical Emacs with xwidgets"))
  (unless (process-live-p grip--process)
    (setq grip--port (markdown-preview-session--free-port))
    ;; Suppress only the premature browser launch.  grip-mode still owns
    ;; source hooks, process lifecycle and refresh-on-save behavior.
    (let ((default-directory (file-name-directory buffer-file-name)))
      (cl-letf (((symbol-function 'grip--preview-1) #'ignore))
        (grip-mode 1))))
  (let ((url (format "http://%s:%d/%s" grip-preview-host grip--port
                     (url-hexify-string
                      (file-name-nondirectory grip--preview-file)))))
    (markdown-preview-session--wait url)
    (markdown-preview-session--display url)))

(provide 'markdown-preview-session)
;;; markdown-preview-session.el ends here
