;;; -*- lexical-binding: t -*-

;; timer report
(require 'benchmark)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(defmacro tt (label &rest forms)
  `(message "%s took %ss" ,label (benchmark-elapse ,@forms)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

;; Garbage Collection (for performance)
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 64 1024 1024)) ;; 64MB

;; Silence logs
;; (setopt byte-compile-warnings '())
(setopt warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)
(setopt inhibit-startup-echo-area-message "")

;; package configuration, using straight instead.
(setopt package-enable-at-startup nil)

;; GUI Elements
(setopt frame-resize-pixelwise t)
(setopt frame-inhibit-implied-resize t)
(setopt frame-title-format '("%b"))
(setopt mode-line-format nil)
(setopt inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(tooltip-mode -1)
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

