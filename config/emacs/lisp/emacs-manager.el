;;; emacs-manager.el --- Third Party Packages  -*- lexical-binding: t; -*-

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setopt package-archive-priorities
        '(("gnu" . 100) ("nongnu" . 80) ("melpa" . 50) ("melpa-stable" . 1)))

;; Native-compile packages at install time (requires native-comp support).
(setopt package-native-compile t)

;; Requires :ensure nil on all built-in packages.
(setopt use-package-always-ensure t)

(provide 'emacs-manager)
;;; emacs-manager.el ends here
