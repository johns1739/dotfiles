;;-*- lexical-binding: t; -*-

;; Loading packages

(when is-simple-editor
  (setq custom-file (concat user-emacs-directory "simple-custom.el"))
  (load (concat user-emacs-directory "packages/simple.el"))
  (load custom-file :no-error-if-file-missing))

(when (not is-simple-editor)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq packages '("setup.el" "emacs.el" "packages.el" "langs.el" "color-themes.el"))
  (dolist (package packages)
    (tt (format "*** %s" package)
        (load (concat user-emacs-directory "packages/" package))))
  (tt (format "*** %s" custom-file)
      (load custom-file :no-error-if-file-missing)))
