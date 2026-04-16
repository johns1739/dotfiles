;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'emacs-base) ;; builtins

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-missing)

(require 'emacs-manager)
(require 'emacs-packages)
(require 'emacs-langs)
(require 'emacs-color-themes)

(provide 'init)
;;; init.el ends here
