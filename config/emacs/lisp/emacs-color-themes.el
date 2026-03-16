;;; emacs-color-themes.el --- Color Themes  -*- lexical-binding: t; -*-

;; https://github.com/emacs-jp/replace-colorthemes?tab=readme-ov-file
;; WARNING: Lexical-Binding
(use-package color-theme-modern)

;; ERROR: Cyclic gnus-group-news-6 foreground face.
(use-package color-theme-sanityinc-tomorrow
  :disabled)

(use-package creamsody-theme)

(use-package darktooth-theme)

(use-package doric-themes)

(use-package ef-themes)

(use-package gruber-darker-theme
  :disabled)

;; ERROR: gnus-group-news-low
(use-package gruvbox-theme
  :disabled)

(use-package kuronami-theme
  :disabled)

(use-package modus-themes)

(use-package nano-theme
  :disabled)

(use-package solarized-theme)

(use-package standard-themes)

(use-package timu-rouge-theme
  :disabled)

(use-package zenburn-theme)

(provide 'emacs-color-themes)
;;; emacs-color-themes.el ends here
