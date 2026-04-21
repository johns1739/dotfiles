;;; early-init.el --- Early Init  -*- lexical-binding: t; -*-

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))

;; Delay garbage collection while Emacs is booting
(setopt gc-cons-threshold most-positive-fixnum)
(setopt gc-cons-percentage 0.6)

;; Single VC backend inscreases booting speed
(setopt vc-handled-backends '(Git))

(setopt inhibit-compacting-font-caches t)

(when (eq system-type 'darwin)
  (setopt ns-use-proxy-icon nil))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setopt warning-minimum-level :error)
(setopt warning-suppress-types '((lexical-binding)))

(setopt package-user-dir (expand-file-name "cache/elpa/" user-emacs-directory))
(setopt package-gnupghome-dir (expand-file-name "cache/elpa/gnupg/" user-emacs-directory))

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 100 1024 1024))
            (setopt gc-cons-percentage 0.1)
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections and %d packages."
             (emacs-init-time "%.2f") gcs-done (length package-activated-list))))

(setopt warning-suppress-types '((comp) (bytecomp) (files)))
(setopt native-comp-async-report-warnings-errors 'silent)
(setopt inhibit-startup-echo-area-message "")

(when (display-graphic-p)
  (setopt use-package-compute-statistics )
  (setopt use-package-verbose t))

(provide 'early-init)
