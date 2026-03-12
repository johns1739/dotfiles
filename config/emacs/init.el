;;-*- lexical-binding: t; -*-

;; TODO: https://github.com/LionyxML/emacs-solo
;; Loading packages

(when is-simple-editor
  (setq custom-file (concat user-emacs-directory "simple-custom.el"))
  (load (concat user-emacs-directory "packages/simple.el") :noerror :nomessage)
  (load custom-file :no-error-if-file-missing :noerror :nomessage))

(when (not is-simple-editor)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq packages '("packages.el" "langs.el" "color-themes.el"))
  (dolist (package packages)
    (load (concat user-emacs-directory "packages/" package)))
  (load custom-file :no-error-if-file-missing))
