;; (defalias 'lsp-ensure-caller #'lsp-deferred "Lsp command to call in major modes.")
(defalias 'lsp-ensure-caller #'eglot-ensure "Lsp command to call in major modes.")

(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)

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
                     :folding t))))
  ;; (setq-default eglot-workspace-configuration
  ;;               '(:solargraph
  ;;                 (;; settings
  ;;                  :useBundler t
  ;;                  :diagnostics t
  ;;                  :completion t
  ;;                  :hover t
  ;;                  :autoformat t
  ;;                  :formatting t
  ;;                  :symbols t
  ;;                  :definitions t
  ;;                  :rename t
  ;;                  :references t
  ;;                  :folding t)))
  )

;; Treesitter
;; Install grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
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


;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :disabled t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 12)
  (lsp-eldoc-render-all t)
  :init
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . lsp-format-buffer)
               ([remap evil-lookup] . lsp-describe-thing-at-point)
               ([remap eldoc] . lsp-describe-thing-at-point)
               ([remap xref-find-references] . lsp-find-references)
               ([remap xref-find-definitions] . lsp-find-definition)
               ([remap evil-goto-definition] . lsp-find-definition)))
  :hook
  (lsp-managed-mode . lsp-set-bindings))
