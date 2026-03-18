;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'emacs-base)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-missing)

(unless is-simple-editor
  (require 'emacs-manager)
  (require 'emacs-packages)
  (require 'emacs-color-themes))

(provide 'init)
;;; init.el ends here
