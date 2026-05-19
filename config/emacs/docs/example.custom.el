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

;; (with-eval-after-load 'abbrev
;;   (define-abbrev global-abbrev-table "eg" "some example here")
;;   (add-hook 'prog-mode-hook #'abbrev-mode))

;; (with-eval-after-load 'auto-dark
;;   (setq auto-dark-themes '((darktooth) (solarized-light))))

;; (with-eval-after-load 'copilot
;;   (add-hook 'prog-mode-hook #'copilot-mode))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `((ruby-mode ruby-ts-mode)
;;                  . ("solargraph" "stdio" :initializationOptions
;;                     ( :useBundler t
;;                       :diagnostics t
;;                       :completion t
;;                       :hover t
;;                       :autoformat :json-false
;;                       :formatting t
;;                       :symbols t
;;                       :definitions t
;;                       :rename t
;;                       :references t
;;                       :folding t))))))

;; (with-eval-after-load 'elfeed
;;   (setopt elfeed-db-directory "~/Documents/elfeed"))

;; (with-eval-after-load 'exec-path-from-shell
;;   (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish"))

;; (with-eval-after-load 'flycheck
;;   (flycheck-add-mode 'javascript-eslint 'web-mode))

;; (with-eval-after-load 'gptel
;;   (setq gptel-model 'claude-opus-4.6
;;         ;; Copilot Business: api.business.githubcopilot.com
;;         ;; Copilot Enterprise: api.enterprise.githubcopilot.com
;;         ;; Copilot Pro and Pro+: api.individual.githubcopilot.com
;;         gptel-backend (gptel-make-gh-copilot "Copilot")))

;; (with-eval-after-load 'org
;;   (setopt org-directory "~/Documents/notes")
;;   (unless (file-exists-p org-directory)
;;     (make-directory org-directory))
;;   (setopt org-agenda-files (list org-directory)))

;; (with-eval-after-load 'project
;;   (project-forget-zombie-projects)
;;   (project-remember-projects-under "~/Projects/"))

;; (with-eval-after-load 'web-mode
;;   (setopt web-mode-css-indent-offset 2)
;;   (setopt web-mode-code-indent-offset 2)
;;   (setopt web-mode-attr-indent-offset 2)
;;   (setopt web-mode-sql-indent-offset 2)
;;   (setopt web-mode-markup-indent-offset 2))

;; (with-eval-after-load 'vterm
;;   (setopt vterm-shell "/opt/homebrew/bin/fish"))

(defun after-init-setup ()
  (if (display-graphic-p)
      (load-theme 'darktooth :no-confirm)
    (progn ;; terminal
      (load-theme 'creamsody :no-confirm)
      (set-face-background 'default "unspecified-bg"))))
(add-hook 'after-init-hook #'after-init-setup)

(server-start)
