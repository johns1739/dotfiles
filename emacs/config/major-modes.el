(use-package go-ts-mode
  :disabled t
  :mode "\\.go\\'"
  :init
  (setq go-ts-mode-indent-offset 4)
  :hook
  (go-ts-mode . eglot-ensure))

(use-package elm-mode
  :disabled t
  :defer t
  :hook
  (elm-mode . eglot-ensure))

(use-package js
  :custom
  (js-indent-level 2))

(use-package erlang
  :disabled t
  :defer t
  :straight nil
  :load-path "/opt/homebrew/lib/erlang/lib/tools-3.6/emacs/"
  :init
  (setq erlang-root-dir "/opt/homebrew/lib/erlang")
  (add-to-list 'exec-path "/opt/homebrew/lib/erlang/bin")
  :hook
  (erlang-mode . eglot-ensure)
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
  (require 'erlang-start))

(use-package yaml-ts-mode
  :defer t
  :mode "\\(\\.yaml\\|.yml\\)\\'"
  :hook
  (yaml-ts-mode . eglot-ensure))

(use-package sqlformat
  :commands (sqlformat)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package geiser-guile
  :commands (geiser-mode))

(use-package expand-region
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))
