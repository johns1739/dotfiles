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
  :defer)

(use-package kaolin-themes
  :disabled
  :defer)

(use-package kuronami-theme
  :disabled ;; nothing really good
  :defer)

(use-package modus-themes
  :defer)

(use-package nano-theme
  :disabled ;; never used
  :defer)

(use-package solarized-theme
  :disabled
  :defer)

(use-package standard-themes
  :defer)

(use-package tao-theme
  :disabled ;; box apperance around methods, does not look nice
  :defer)

(use-package timu-rouge-theme
  :disabled ;; dont like the underline
  :defer)

(use-package zenburn-theme
  :defer)

(provide 'emacs-color-themes)
;;; emacs-color-themes.el ends here
