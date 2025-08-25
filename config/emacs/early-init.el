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
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message 't)

;; package configuration, using straight instead.
(setq package-enable-at-startup nil)

;; GUI Elements
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq frame-title-format '("Emacs"))
(setq mode-line-format nil)
(setq inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(tooltip-mode -1)
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

