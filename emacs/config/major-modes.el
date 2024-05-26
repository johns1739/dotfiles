(use-package go-ts-mode
  :disabled t
  :mode "\\.go\\'"
  :init
  (setq go-ts-mode-indent-offset 4)
  (defun lsp-go-save-hooks ()
    (setq tab-width 4)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (setq lsp-go-analyses '((shadow . t)
                          (unusedvariable . t)
                          (unusedwrite . t)
                          (useany . t)
                          (fieldalignment . t)))
  :hook
  (go-ts-mode . lsp-go-save-hooks)
  (go-ts-mode . lsp-deferred))

(use-package elm-mode
  :disabled t
  :defer t
  :hook
  (elm-mode . eglot-ensure))

(use-package erlang
  :disabled t
  :defer t
  :straight nil
  :load-path "/opt/homebrew/lib/erlang/lib/tools-3.6/emacs/"
  :init
  (setq erlang-root-dir "/opt/homebrew/lib/erlang")
  (add-to-list 'exec-path "/opt/homebrew/lib/erlang/bin")
  :hook
  (erlang-mode . lsp-deferred)
  :mode
  (("\\.erl?$" . erlang-mode)
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
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]erl_crash.dump\\'")
  (require 'erlang-start))

(use-package lsp-haskell
  :disabled t
  :defer t
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-literal-mode . lsp-deferred))

(use-package yaml-ts-mode
  :defer t
  :mode "\\(\\.yaml\\|.yml\\)\\'"
  :hook
  (yaml-ts-mode . lsp-deferred))

(use-package sqlformat
  :commands (sqlformat)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package geiser-guile
  :disabled t
  :commands (geiser-mode))
