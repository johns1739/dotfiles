;;; external-package-manager.el --- Third Party Packages  -*- lexical-binding: t; -*-

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

(setopt package-archive-priorities
        '(("gnu" . 100) ("melpa-stable" . 50) ("melpa" . 1)))

(setopt use-package-always-ensure t)

(provide 'emacs-external-manager)
