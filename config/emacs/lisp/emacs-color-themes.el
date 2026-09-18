;;; emacs-color-themes.el --- Color Themes  -*- lexical-binding: t; -*-

;; Themes are installed but deferred — loaded on demand via `load-theme'.
;; Previously tried and rejected:
;;   - color-theme-modern: too many themes, hard to remember
;;   - color-theme-sanityinc-tomorrow: cyclic face definition error
;;   - gruber-darker-theme: -
;;   - kaolin-themes: -
;;   - kuronami-theme: nothing compelling
;;   - nano-theme: never used
;;   - solarized-theme: -
;;   - tao-theme: box appearance around methods
;;   - timu-rouge-theme: intrusive underlines

(use-package modus-themes
  :defer
  :hook (modus-themes-after-load-theme . terminal-reset-background)
  :init
  (defun terminal-reset-background ()
    "Drops background face when terminal mode."
    (interactive)
    (unless (display-graphic-p)
      (set-face-background 'default "unspecified-bg"))))

(use-package creamsody-theme :defer)
(use-package darktooth-theme :defer)
(use-package doric-themes :defer)
(use-package ef-themes :defer)
(use-package gruber-darker-theme :defer)
(use-package gruvbox-theme :defer)
(use-package miasma-theme :defer)

(use-package standard-themes :defer)
(use-package zenburn-theme :defer)

(provide 'emacs-color-themes)
;;; emacs-color-themes.el ends here
