;;; emacs-color-themes.el --- Color Themes  -*- lexical-binding: t; -*-

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (cond (is-simple-editor
;;                    (load-theme 'wombat :no-confirm))
;;                   ((display-graphic-p)
;;                    (load-theme 'darktooth :no-confirm))
;;                   (t
;;                    (load-theme 'wombat :no-confirm)))))

;; https://github.com/emacs-jp/replace-colorthemes?tab=readme-ov-file
;; WARNING: Lexical-Binding
(use-package color-theme-modern
  :defer)

;; ERROR: Cyclic gnus-group-news-6 foreground face.
(use-package color-theme-sanityinc-tomorrow
  :disabled)

(use-package creamsody-theme
  :defer)

(use-package darktooth-theme
  :defer)

(use-package doric-themes
  :defer)

(use-package ef-themes
  :defer)

(use-package gruber-darker-theme
  :disabled)

;; ERROR: gnus-group-news-low
(use-package gruvbox-theme
  :disabled)

(use-package kuronami-theme
  :disabled)

(use-package modus-themes
  :defer)

(use-package nano-theme
  :disabled)

(use-package solarized-theme
  :defer)

(use-package standard-themes
  :defer)

(use-package timu-rouge-theme
  :disabled)

(use-package zenburn-theme
  :defer)

(provide 'emacs-color-themes)
;;; emacs-color-themes.el ends here
