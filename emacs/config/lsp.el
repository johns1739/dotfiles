(use-package emacs
  :init
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
                       :folding t)))))
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
            (sql "https://github.com/DerekStride/tree-sitter-sql")))))
