;; timer report
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

;; Garbage Collection (for performance)
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 128 1024 1024)) ;; 128MB

;; Silence logs
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))

;; package configuration
(setq package-enable-at-startup nil)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (customize-set-variable
   'package-archive-priorities
   '(("gnu"    . 99)   ; prefer GNU packages
     ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
     ("stable" . 70)   ; prefer "released" versions from melpa
     ("melpa"  . 0))))  ; if all else fails, get it from melpa

;; remove gui elements
(menu-bar-mode -1)
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))
