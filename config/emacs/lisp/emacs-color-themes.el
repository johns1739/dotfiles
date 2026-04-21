;;; emacs-color-themes.el --- Color Themes  -*- lexical-binding: t; -*-

;; https://github.com/emacs-jp/replace-colorthemes?tab=readme-ov-file
;; WARNING: Lexical-Binding
(use-package color-theme-modern
  :disabled ;; Too many themes, hard to remember
  :defer)


(use-package color-theme-sanityinc-tomorrow
  :disabled ;; ERROR: Cyclic gnus-group-news-6 foreground face.
  :defer)

(use-package creamsody-theme
  :defer)

(use-package darktooth-theme
  :defer)

(use-package doric-themes
  :defer)

(use-package ef-themes
  :defer)

(use-package gruber-darker-theme
  :disabled
  :defer)

(use-package gruvbox-theme
  :disabled ;; ERROR: gnus-group-news-low
  :defer)

(use-package kuronami-theme
  :disabled
  :defer)

(use-package modus-themes
  :defer)

(use-package nano-theme
  :disabled ;; never used
  :defer)

(use-package solarized-theme
  :defer)

(use-package standard-themes
  :defer)

(use-package timu-rouge-theme
  :disabled ;; dont like the underline
  :defer)

(use-package zenburn-theme
  :defer)

(provide 'emacs-color-themes)
;;; emacs-color-themes.el ends here
