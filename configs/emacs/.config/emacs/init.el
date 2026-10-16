;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'emacs-builtins)
(require 'emacs-manager)
(require 'emacs-packages)
(require 'emacs-langs)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-missing)

(provide 'init)
;;; init.el ends here
