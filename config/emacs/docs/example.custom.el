;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-frame-alist
   '((fullscreen . maximized))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t ( :family "JetBrainsMonoNL Nerd Font Mono"
                 :foundry "nil"
                 :weight light
                 :slant normal
                 :width normal
                 :height 130)))))

;; (add-hook 'prog-mode-hook #'copilot-mode)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'indent-bars-mode)

;; (with-eval-after-load 'vterm
;;   (setopt vterm-shell "/opt/homebrew/bin/fish"))

;; (with-eval-after-load 'elfeed
;;   (setopt elfeed-db-directory "~/Documents/elfeed"))

;; (with-eval-after-load 'auto-dark
;;   (setq auto-dark-themes '((darktooth) (solarized-light))))

(with-eval-after-load 'org
  (setopt org-directory "~/Documents/notes")
  (unless (file-exists-p org-directory)
    (make-directory org-directory))
  (setopt org-agenda-files (list org-directory)))

(defun after-init-setup ()
  (if (display-graphic-p)
      (load-theme 'darktooth :no-confirm)
    (progn ;; terminal
      (load-theme 'creamsody :no-confirm)
      (set-face-background 'default "unspecified-bg"))))
(add-hook 'after-init-hook #'after-init-setup)
