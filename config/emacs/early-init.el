;;; -*- lexical-binding: t -*-

(defvar is-simple-editor (getenv "SIMPLE")
  "Whether Emacs is running in a simple editor environment.")

(setopt package-enable-at-startup nil)

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'tooltip-mode)
    (tooltip-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Garbage Collection (for performance)
(setopt gc-cons-percentage 0.1)
(setopt gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setopt read-process-output-max (* 1024 1024)) ;; 1mb

(unless is-simple-editor
  (require 'benchmark)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t)
  (setopt frame-resize-pixelwise t)
  (setopt frame-inhibit-implied-resize t)
  (setopt frame-title-format '("%n %b - %F"))
  (setopt inhibit-compacting-font-caches t)
  (setopt mode-line-format nil)

  ;; Silence logs
  ;; (setopt byte-compile-warnings '())
  (setopt warning-suppress-log-types '((comp) (bytecomp)))
  (setopt native-comp-async-report-warnings-errors 'silent)
  (setopt inhibit-startup-echo-area-message "")

  (defmacro tt (label &rest forms)
    `(message "%s took %ss" ,label (benchmark-elapse ,@forms)))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message
               "*** Emacs loaded in %s seconds with %d garbage collections."
               (emacs-init-time "%.2f") gcs-done))))
