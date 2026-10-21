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

(use-package exec-path-from-shell
  :demand ;; Must be loaded first before other packages.
  :if (or (daemonp)
          (and (display-graphic-p) (memq window-system '(mac ns x pgtk))))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(provide 'emacs-manager)
;;; emacs-manager.el ends here
