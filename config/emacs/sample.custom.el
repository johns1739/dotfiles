;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMonoNL Nerd Font Mono" :foundry "nil" :slant normal :weight light :height 130 :width normal)))))

;; Color theme

(if (display-graphic-p)
    (load-theme 'gruvbox-dark-hard :no-confirm-loading)
  (progn
    (load-theme 'modus-vivendi-tritanopia :no-confirm-loading)
    (set-face-background 'default "unspecified")))
