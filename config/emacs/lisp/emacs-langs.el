;;; emacs-langs.el --- Language Modes  -*- lexical-binding: t; -*-

(use-package conf-mode
  :ensure nil
  :hook
  (conf-mode . display-line-numbers-mode)
  (conf-mode . outline-minor-mode)
  :mode ("\\.env\\..*\\'" "\\.env\\'"))

(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'" . css-ts-mode)
  :custom
  (css-indent-offset 2))

(use-package elisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (emacs-lisp-mode . electric-pair-local-mode)
  :bind
  ( :map emacs-lisp-mode-map
    ("C-c t" . ielm)))

(use-package elixir-ts-mode
  :ensure nil
  :mode "\\.exs?\\'"
  :init
  (defun elixir-ts-mode-setup ()
    (cond
     ((string-match-p "router.ex$" (buffer-name))
      (setq-local outline-regexp " *\\(get\\|delete\\|put\\|post\\|scope\\|pipe_through\\|resources\\) "))
     ((string-match-p "_test.exs$" (buffer-name))
      (setq-local outline-regexp " *\\(describe \\|test \\|setup \\)"))))
  :hook
  (elixir-ts-mode . elixir-ts-mode-setup)
  (elixir-ts-mode . prettify-symbols-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) "expert_lsp" "--stdio")))
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    (add-to-list 'compilation-error-regexp-alist 'elixir-unit-test-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-unit-test-target "     \\([^ ]+\\.exs\\):\\([0-9]+\\)" 1 2 nil 1 1))
    (add-to-list 'compilation-error-regexp-alist 'elixir-error-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-error-target "    error:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 1 1))
    (add-to-list 'compilation-error-regexp-alist 'elixir-warning-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-warning-target "    warning:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 2 1))))

(use-package elm-mode
  ;; Dependencies
  ;; npm install -g @elm-tooling/elm-language-server
  ;; npm install -g elm elm-test elm-format
  ;; npm install -g elm-review
  :mode "\\.elm\\'")

(use-package gleam-ts-mode
  ;; https://github.com/gleam-lang/tree-sitter-gleam
  ;; NOTE: Resolve issue with:
  ;; https://github.com/gleam-lang/gleam-mode/commit/ae8aecda23e9dca755d80e86cdb7c336011c2321
  ;; Install
  ;; (gleam-ts-install-grammar)
  :mode (rx ".gleam" eos)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(gleam-ts-mode "gleam" "lsp")))
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))

(use-package go-ts-mode
  ;; Install LSP:
  ;; go install golang.org/x/tools/gopls@latest
  :ensure nil
  :mode "\\.go\\'"
  :mode ("go\\.mod\\'" . go-mod-ts-mode))

(use-package janet-ts-mode
  :mode "\\.janet\\'"
  :vc ( :url "https://github.com/sogaiu/janet-ts-mode" :rev :newest)
  :hook (janet-ts-mode . electric-pair-local-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(janet-mode . janet-ts-mode))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(janet-ts-mode "janet-lsp"))))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-ts-mode)
  :init
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
  (json-ts-mode . js-ts-mode-setup))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-view-mode)
  :hook
  (gfm-view-mode . visual-line-mode)
  (markdown-view-mode . visual-line-mode)
  :bind ( :map markdown-mode-command-map
          ("a" . markdown-table-align)
          :map markdown-mode-map
          ("M-;" . markdown-blockquote-region)
          ("M-H" . markdown-mark-block)
          ("C-c C-e" . gfm-mode)
          ("C-c C-v" . gfm-view-mode)
          ("C-c C-n" . markdown-outline-next)
          ("C-c C-p" . markdown-outline-previous)
          ("C-c C-." . markdown-do))
  :custom
  (markdown-command "multimarkdown"))

(use-package python
  ;; Example .dir-locals.el to configure compile command.
  ;; ((python-mode . ((eval . (if (and (buffer-file-name)
  ;;                                   (string-match-p "test_.*\\.py" (file-name-nondirectory (buffer-file-name))))
  ;;                              (setq-local compile-command (concat "pytest " (relative-file-name))))))))
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :init
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
  :mode ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'" ".irbrc\\'")
  :interpreter "ruby"
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil)
  :init
  (defun ruby-ts-mode-setup ()
    (cond ((and (buffer-file-name)
                (string-match-p ".+_spec.rb" (file-name-nondirectory (buffer-file-name))))
           (progn
             (setq-local compile-command `(concat "bundle exec rspec "
                                                  (relative-file-name)
                                                  (if (> (line-number-at-pos) 10) (format ":%d" (line-number-at-pos)))))
             (setq-local outline-search-function nil)
             (setq-local outline-regexp " +\\(context \\|describe \\|test \\|it \\)")))
          ((buffer-file-name)
           (setq-local compile-command `(concat "bundle exec rubocop -a --force-exclusion " (relative-file-name))))))
  :hook
  (ruby-ts-mode . ruby-ts-mode-setup)
  :config
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    ;;   TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
    (add-to-list 'compilation-error-regexp-alist 'ruby-rspec-inner-failure-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(ruby-rspec-inner-failure-target
                   "# \\([^:]+\\):\\([0-9]+\\):"
                   1 2 nil nil 1))
    (add-to-list 'compilation-error-regexp-alist 'ruby-rspec-test-failure-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(ruby-rspec-test-failure-target
                   "^rspec \\([^:]+\\):\\([0-9]+\\)"
                   1 2 nil nil 1))))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 2))

(use-package sql
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :init
  (defun sql-mode-setup ()
    ;; 2-spaces, no-grouping
    (setq format-all-formatters '(("SQL" (pgformatter "-s2" "-g")))))
  :hook
  (sql-mode . sql-mode-setup))

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook
  (typescript-ts-mode . js-ts-mode-setup)
  (tsx-ts-mode . js-ts-mode-setup)
  :custom
  (typescript-indent-level 2))

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package vue-ts-mode
  :disabled ;; embedded-langs are not rendered
  ;; Dependencies
  ;; npm install -g @vue/language-server typescript-language-server
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(vue "https://github.com/ikatyang/tree-sitter-vue")))
  (defun vue-eglot-tsdk-option ()
    (let* ((npm-root (string-trim (shell-command-to-string "npm root -g")))
           (tsdk-path (concat npm-root "/typescript/lib")))
      (concat "--tsdk=" tsdk-path)))
  :bind
  ( :map vue-ts-mode-map
    ("M-o" . nil))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(vue-ts-mode "vue-language-server" "--stdio" ,(vue-eglot-tsdk-option)))))

(use-package web-mode
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  ;; :mode "\\.vue\\'"
  :custom
  (web-mode-enable-auto-indentation nil))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

(provide 'emacs-langs)
;;; emacs-langs.el ends here
