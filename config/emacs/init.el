;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'emacs-base)

(unless is-simple-editor
  (require 'emacs-manager)
  (require 'emacs-packages)
  (require 'emacs-color-themes)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file :no-error-if-file-missing))

(when is-simple-editor
  (setq custom-file (concat user-emacs-directory ".custom.el"))
  (load custom-file :no-error-if-file-missing))

(provide 'init)
;;; init.el ends here
