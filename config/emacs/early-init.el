;;; early-init.el --- Early Init  -*- lexical-binding: t; -*-

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Prevent the frame from resizing when toggling UI elements above
(setopt frame-inhibit-implied-resize t)

;; Default frame parameters to avoid flicker before theme loads
(setopt default-frame-alist
        '((vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)))

;; Delay garbage collection while Emacs is booting
(setopt gc-cons-threshold most-positive-fixnum)
(setopt gc-cons-percentage 0.6)

;; Disable file-name-handler-alist during init (restored after startup)
(defvar jb/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Single VC backend increases booting speed
(setopt vc-handled-backends '(Git))

(setopt inhibit-compacting-font-caches t)

(when (eq system-type 'darwin)
  (setopt ns-use-proxy-icon nil))

;; Do not native compile if on battery power (Emacs 31+)
(when (boundp 'native-comp-async-on-battery-power)
  (setopt native-comp-async-on-battery-power nil))

;; Suppress noisy warnings during init
(setopt warning-minimum-level :error)
(setopt warning-suppress-types '((lexical-binding) (comp) (bytecomp) (files)))

(setopt package-user-dir (expand-file-name "cache/elpa/" user-emacs-directory))
(setopt package-gnupghome-dir (expand-file-name "cache/elpa/gnupg/" user-emacs-directory))

(setopt native-comp-async-report-warnings-errors 'silent)

;; Schedule sensible defaults for after booting
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 100 1024 1024))
            (setopt gc-cons-percentage 0.1)
            ;; Restore file-name-handler-alist
            (setq file-name-handler-alist jb/file-name-handler-alist)
            ;; Relax warning level after init
            (setopt warning-minimum-level :warning)
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections and %d packages."
             (emacs-init-time "%.2f") gcs-done (length package-activated-list))))

(setopt use-package-compute-statistics t)
(setopt use-package-verbose t)

(provide 'early-init)
;;; early-init.el ends here
