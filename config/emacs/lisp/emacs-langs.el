;;; emacs-langs.el --- Language Modes  -*- lexical-binding: t; -*-

(use-package bash-ts-mode
  :ensure nil
  :mode "\\.\\(sh\\|bash\\)\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(bash "https://github.com/tree-sitter/tree-sitter-bash" "master" "src"))))

(use-package conf-mode
  :ensure nil
  :hook
  (conf-mode . display-line-numbers-mode)
  :mode ("\\.env\\..*\\'" "\\.env\\'"))

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(css "https://github.com/tree-sitter/tree-sitter-css"))))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "Dockerfile.*\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))))

(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :bind
  ( :map emacs-lisp-mode-map
    ("C-c t" . ielm)))

(use-package elixir-ts-mode
  :ensure nil
  :mode "\\.exs?\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")))
  (defun elixir-ts-mode-setup ()
    (setq-local compilation-error-regexp-alist '(elixir-unit-test-target elixir-error-target elixir-warning-target))
    (cond
     ((string-match-p "router.ex$" (buffer-name))
      (setq outline-regexp " *\\(get\\|delete\\|put\\|post\\|scope\\|pipe_through\\|resources\\) "))
     ((string-match-p "_test.exs$" (buffer-name))
      (setq outline-regexp " *\\(describe \\|test \\|setup \\)"))))
  :hook
  (elixir-ts-mode . elixir-ts-mode-setup)
  (elixir-ts-mode . prettify-symbols-mode)
  :config
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-unit-test-target "     \\([^ ]+\\.exs\\):\\([0-9]+\\)" 1 2 nil 1 1))
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-error-target "    error:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 1 1))
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-warning-target "    warning:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 2 1))))

(use-package go-ts-mode
  ;; Install LSP:
  ;; go install golang.org/x/tools/gopls@latest
  :ensure nil
  :mode "\\.go\\'"
  :mode ("go\\.mod\\'" . go-mod-ts-mode)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(go "https://github.com/tree-sitter/tree-sitter-go"))
    (add-to-list 'treesit-language-source-alist
                 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(heex "https://github.com/phoenixframework/tree-sitter-heex"))))

(use-package js-ts-mode
  :ensure nil
  :mode "\\.jsx?\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (add-to-list 'treesit-language-source-alist
                 '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))
  (defun js-ts-mode-setup ()
    (setq indent-tabs-mode nil))
  :hook
  (js-ts-mode . js-ts-mode-setup)
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :hook
  (json-ts-mode . js-ts-mode-setup)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(json "https://github.com/tree-sitter/tree-sitter-json"))))

(use-package markdown-ts-mode
  ;; :ensure nil ;; only available in latest emacs.
  :mode "\\.md\\'"
  :mode ("README\\.md\\'" . gfm-mode) ;; depends on builtin markdown-mode
  :bind ( :map markdown-ts-mode-map
          ("M-;" . markdown-blockquote-region)
          ("M-H" . markdown-mark-block)
          ("M-n" . markdown-outline-next)
          ("M-p" . markdown-outline-previous)
          ("C-c C-." . markdown-do))
  :custom
  (markdown-command "multimarkdown")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
    (add-to-list 'treesit-language-source-alist
                 '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))))

(use-package python
  ;; Example .dir-locals.el to configure compile command.
  ;; ((python-mode . ((eval . (if (and (buffer-file-name)
  ;;                                   (string-match-p "test_.*\\.py" (file-name-nondirectory (buffer-file-name))))
  ;;                              (setq-local compile-command (concat "pytest " (relative-file-name))))))))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :custom
  (python-indent-guess-indent-offset-verbose t)
  (python-indent-offset 4)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(python "https://github.com/tree-sitter/tree-sitter-python")))
  (defun python-ts-mode-setup ()
    (when (and (buffer-file-name)
               (string-match-p "test_.*\\.py" (file-name-nondirectory (buffer-file-name))))
      (setq-local outline-regexp "\s*\\(\\(async\\)? def test_\\|class Test\\)")
      (setq-local compile-command (concat "pytest " (relative-file-name)))))
  :hook
  (python-ts-mode . python-ts-mode-setup)
  :config
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist 'python-pytest-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(python-pytest-target
                   "^\\([A-Za-z0-9/][^ (]+\\.py\\):\\([1-9][0-9]*\\): "
                   1 2 nil nil 1))))

(use-package ruby-ts-mode
  :ensure nil
  :mode ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'")
  :interpreter "ruby"
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
               '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")))
  (defun ruby-ts-mode-setup ()
    (setq-local outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)"))
  :hook
  (ruby-ts-mode . ruby-ts-mode-setup)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((ruby-ts-mode)
                   . ("solargraph" "stdio" :initializationOptions
                      ( :useBundler t
                        :diagnostics t
                        :completion t
                        :hover t
                        :autoformat :json-false
                        :formatting t
                        :symbols t
                        :definitions t
                        :rename t
                        :references t
                        :folding t))))))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-indent-level 2)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src"))))

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src"))))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :hook
  (typescript-ts-mode . js-ts-mode-setup)
  :custom
  (typescript-indent-level 2)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'"
  :hook
  (tsx-ts-mode . js-ts-mode-setup)
  :custom
  (typescript-indent-level 2)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))

(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(vue "https://github.com/ikatyang/tree-sitter-vue"))))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src"))))

(provide 'emacs-langs)
;;; emacs-langs.el ends here
