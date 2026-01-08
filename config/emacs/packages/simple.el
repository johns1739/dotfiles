;;; -*- lexical-binding: t -*-

(setopt completion-auto-help 'always)
(setopt completion-auto-select t)
(setopt completion-category-defaults nil)
(setopt completion-category-overrides '((file (styles . (basic partial-completion)))))
(setopt completion-cycle-threshold 3)
(setopt completion-ignore-case t)
(setopt completion-styles '(basic substring partial-completion))
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-max-height 20)
(setopt duplicate-line-final-position 1)
(setopt inhibit-startup-message t)
(setopt ring-bell-function 'ignore)
(setopt tab-always-indent t)
(setopt use-short-answers t)

(global-so-long-mode t)

(use-package hippie-exp
  :bind (:map global-map ("M-i" . hippie-expand))
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-list
     try-expand-line
     try-expand-dabbrev
     ;; try-expand-list-all-buffers
     try-expand-line-all-buffers
     try-expand-dabbrev-all-buffers
     ;; try-expand-whole-kill ;; use M-y instead
     ;; try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name))
  :init
  (defun hippie-expand-case-fold-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-fold-advice))
