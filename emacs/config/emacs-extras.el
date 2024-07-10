;; Hooks
(add-hook 'before-save-hook #'whitespace-cleanup)
;; (with-eval-after-load 'ispell
;;   (when (executable-find ispell-program-name)
;;     (add-hook 'text-mode-hook #'flyspell-mode)
;;     (add-hook 'prog-mode-hook #'flyspell-prog-mode)))
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook #'hl-line-mode)

;; Modes
(auto-save-visited-mode -1) ;; Annoying with whitespace cleanup constantly moving the point
(column-number-mode +1)
(delete-selection-mode -1)
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP
(electric-indent-mode +1)
(electric-pair-mode +1)
(global-auto-revert-mode +1)
(global-eldoc-mode +1)
(global-so-long-mode t)
(line-number-mode +1)
(recentf-mode 1)
(repeat-mode -1) ;; Sometimes gets in the way.
(save-place-mode 1)
(savehist-mode 1)
(window-divider-mode -1)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((ruby-mode ruby-ts-mode)
                 . ("solargraph" "stdio" :initializationOptions
                    (;; options
                     :useBundler t
                     :diagnostics t
                     :completion t
                     :hover t
                     :autoformat :json-false
                     :formatting t
                     :symbols t
                     :definitions t
                     :rename t
                     :references t
                     :folding t)))))

(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (haskell "https://github.com/tree-sitter/haskell-tree-sitter")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (sql "https://github.com/DerekStride/tree-sitter-sql"))))

;; (set-face-font 'default "-*-Hack Nerd Font-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-*-Roboto Mono-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-*-JetBrainsMono Nerd Font-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-*-Monaspace Neon-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-*-Monaspace Argon-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height (car toggle-big-font-sizes)
                    :weight 'light ;; thin, light, medium, regular
                    :slant 'normal
                    :width 'normal)
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 112))
