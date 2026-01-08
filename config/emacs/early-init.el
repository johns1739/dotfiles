;;; -*- lexical-binding: t -*-


(defvar is-simple-editor (getenv "SIMPLE")
  "Whether Emacs is running in a simple editor environment.")

(setopt package-enable-at-startup nil)
(menu-bar-mode -1)
(tooltip-mode -1)
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

;; Garbage Collection (for performance)
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 64 1024 1024)) ;; 64MB

(unless is-simple-editor
  (require 'benchmark)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setopt frame-resize-pixelwise t)
  (setopt frame-inhibit-implied-resize t)
  (setopt frame-title-format '("%b"))
  (setopt inhibit-compacting-font-caches t)
  (setopt mode-line-format nil)
  
  ;; Silence logs
  ;; (setopt byte-compile-warnings '())
  (setopt warning-suppress-log-types '((comp) (bytecomp)))
  (setopt native-comp-async-report-warnings-errors 'silent)
  (setopt inhibit-startup-echo-area-message "")
  
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
  (setenv "LSP_USE_PLISTS" "true")
  
  (defmacro tt (label &rest forms)
    `(message "%s took %ss" ,label (benchmark-elapse ,@forms)))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message
               "*** Emacs loaded in %s seconds with %d garbage collections."
               (emacs-init-time "%.2f") gcs-done))))



