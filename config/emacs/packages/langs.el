;;; -*- lexical-binding: t -*-

;; (load (locate-user-emacs-file "packages/langs.el") :no-error-if-file-is-missing)

(use-package cc-mode
  :mode "\\.c\\'"
  :straight nil
  :init
  (add-to-list 'major-mode-remap-alist '(cc-mode . c-ts-mode)))

(use-package common-lisp-mode
  :straight nil
  :mode
  (("\\.lisp$" . common-lisp-mode)
   ("\\.clisp$" . common-lisp-mode))
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el") t) ;; t = noerror
  (setq inferior-lisp-program "sbcl"))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2))

(use-package dockerfile-mode
  :mode (("\\Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode)))

(use-package elixir-ts-mode
  ;; Setup
  ;; (add-to-list 'exec-path "~/.local/elixir-ls")
  :mode (("\\.ex$" . elixir-ts-mode)
         ("\\.exs$" . elixir-ts-mode)
         ("\\.heex$" . heex-ts-mode))
  :init
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    (add-to-list 'compilation-error-regexp-alist 'elixir-warning-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-warning-target
                   "└─ \\([^:]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 1 1)))
  (defun elixir-compile ()
    (interactive)
    (setq compile-command
          (cond ((string-match-p "_test.exs\\'" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (relative-file-name)))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list "mix test " file-name))
                     (string-join (list "mix test " (format "%s:%s" file-name linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))
  (defun elixir-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'elixir-compile))
  (defun elixir-setup ()
    (cond
     ((string-match-p "router.ex$" (buffer-name))
      (setq outline-regexp
            " *\\(get\\|delete\\|put\\|post\\|scope\\|pipe_through\\|resources\\) "))
     ((string-match-p "_test.exs$" (buffer-name))
      (setq outline-regexp " *\\(describe \\|test \\|setup \\)")))
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . elixir-compile)
               ([remap comint-dwim] . elixir-comint)))
  :hook
  (elixir-ts-mode . elixir-setup)
  :config
  ;; TODO: Enable more features
  ;; https://joaotavora.github.io/eglot/#User_002dspecific-configuration-1
  ;; https://github.com/elixir-lsp/elixir-ls?tab=readme-ov-file#elixirls-configuration-settings
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-ts-mode heex-ts-mode) .
                   ,(eglot-alternatives '("language_server.sh" "start_lexical.sh"))))))

(use-package erlang
  :disabled
  ;; https://adoptingerlang.org/docs/development/setup/#emacs
  :load-path "/Users/juan/.asdf/installs/erlang/27.3/lib/tools-4.1.1/emacs"
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode))
  :config
  (add-to-list 'eglot-server-programs
               '(erlang-mode "elp" "server")))

(use-package geiser-guile
  :commands (geiser geiser-mode)
  :custom
  (geiser-debug-jump-to-debug t)
  :hook
  (scheme-mode . geiser-mode))

(use-package gleam-ts-mode
  :straight (:host github :repo "gleam-lang/gleam-mode")
  :mode (rx ".gleam" eos)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(gleam-ts-mode "gleam" "lsp"))))

(use-package go-ts-mode
  ;; go install golang.org/x/tools/gopls@latest
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4))

(use-package janet-mode
  :mode "\\.janet$"
  :config
  (exec-path-from-shell-copy-env "JANET_PATH")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(janet-mode "janet-lsp"))))

(use-package js-mode
  :straight nil
  :mode "\\.js\\'"
  :custom
  (js-indent-level 4))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package make-mode
  :straight nil
  :init
  (defun make-mode-setup ()
    (setq-local outline-regexp "^[A-Za-z].+:"))
  :hook
  (makefile-bsdmake-mode . make-mode-setup))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("M-H" . markdown-mark-block)
              ("M-n" . markdown-outline-next)
              ("M-p" . markdown-outline-previous)
              ("C-c C-." . markdown-do)))

(use-package openapi-preview
  ;; requirements:
  ;; npm i -g redoc-cli
  :if (executable-find "redoc-cli")
  :straight (:host github :repo "merrickluo/openapi-preview")
  :bind (:map yaml-ts-mode-map
              ("C-c C-c p" . openapi-preview)))

(use-package python-mode
  ;; setup
  ;; pip install "pyright[nodejs]"
  :straight nil
  :mode "\\.py$"
  :init
  ;; Run in init because :config does not execute
  ;; for whatever reason.
  ;; Disabled ts because of gcc/libgccjit issues in emacs.
  ;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (defun python-setup ()
    (setq-local tab-width 4))
  :hook
  (python-ts-mode . python-setup)
  (python-mode . python-setup)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4))

(use-package pyvenv
  ;; Add to .envrc the virtualenv folder to load per project (example):
  ;; echo 'export WORKON_HOME="$HOME/pathToProj/.venv"' >> .envrc
  :config
  ;; mode line is not a custom option
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[" "venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-tracking-mode t))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (defun rails-compile ()
    (interactive)
    (setq compile-command
          (cond ((string-match-p "_test.rb\\'" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (relative-file-name)))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list "rails t " file-name))
                     (string-join (list "rails t " (s-concat file-name ":" linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))
  (defun rails-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'rails-compile))
  (defun ruby-setup ()
    (setq-local compile-command "rails t")
    (setq-local outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)")
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . rails-compile)
               ([remap comint-dwim] . rails-comint)))
  :hook
  (ruby-base-mode . ruby-setup)
  :config
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist 'rails-test-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(rails-test-target
                   "^rails test \\([^:]+\\):\\([0-9]+\\)" 1 2 nil nil 1))
    (add-to-list 'compilation-error-regexp-alist 'rspec-backtrace-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(rspec-backtrace-target
                   "^ +# \\(./[A-Za-z0-9][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))
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
                       :folding t))))))

(use-package terraform-mode
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2))

(use-package typescript-ts-mode
  :mode "\\.ts$"
  :custom
  (typescript-ts-mode-indent-offset 4)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode) . ("deno" "lsp")))))

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package yaml-mode
  :mode "\\(\\.ya?ml\\)\\'")

(use-package zig-mode
  :mode "\\.zig\\'")
