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

(with-eval-after-load 'gptel
  (setq gptel-model 'claude-opus-4.6)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")))

(add-hook 'prog-mode-hook #'copilot-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'indent-bars-mode)

(add-to-list 'exec-path "~/.local/elixir-ls")

(if (display-graphic-p)
    (load-theme 'darktooth :no-confirm-loading)
  (progn
    (load-theme 'creamsody :no-confirm-loading)
    (set-face-background 'default "unspecified-bg")))
