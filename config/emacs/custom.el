(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "nil" :slant normal :weight regular :height 130 :width normal)))))

;; Color theme
(load-theme 'ef-autumn :no-confirm-loading)

(setq find-program 'df)

;; Note Settings
(setq notes-directory "~/workspace/notes")
(setq org-directory notes-directory)
(setq org-agenda-files (list org-directory))
(setq denote-directory notes-directory)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 112)))
