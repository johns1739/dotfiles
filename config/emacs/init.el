;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'emacs-builtins)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-missing)

(require 'emacs-manager)
(require 'emacs-color-themes)
(require 'emacs-packages)
(require 'emacs-langs)

(provide 'init)
;;; init.el ends here
