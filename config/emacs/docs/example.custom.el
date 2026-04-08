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

(add-hook 'prog-mode-hook #'copilot-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'indent-bars-mode)

(with-eval-after-load 'elfeed
  (setopt elfeed-db-directory "~/Documents/elfeed"))

(with-eval-after-load 'gptel
  (setq gptel-model 'claude-opus-4.6)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")))

(with-eval-after-load 'org
  (setq org-directory "~/Documents/notes"))

(add-hook 'after-init-hook
          (lambda ()
            (cond ((display-graphic-p) ;; graphics
                   (add-hook 'elixir-ts-mode-hook #'prettify-symbols-mode)
                   (load-theme 'darktooth :no-confirm))
                  ((and (not (display-graphic-p)) (not is-simple-editor)) ;; terminal
                   (load-theme 'creamsody :no-confirm)
                   (set-face-background 'default "unspecified-bg"))
                  (is-simple-editor ;; simple terminal
                   (load-theme 'wombat :no-confirm)))))
