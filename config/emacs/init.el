;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for Emacs configuration.  Modules are loaded from the
;; `lisp/' subdirectory in the following order:
;;   emacs-base         — built-in settings and sane defaults
;;   emacs-manager      — package archive and use-package setup
;;   emacs-color-themes — theme and appearance configuration
;;   emacs-packages     — third-party package declarations
;;   emacs-langs        — language-specific modes and tooling

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Built-in defaults (no package dependencies)
(require 'emacs-base)

;; Persist Customize settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-missing)

;; Package management setup (archives, use-package)
(require 'emacs-manager)

;; Theme early to minimize flash of unstyled content
(require 'emacs-color-themes)

;; Third-party packages and language support
(require 'emacs-packages)
(require 'emacs-langs)

(provide 'init)
;;; init.el ends here
