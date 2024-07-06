(use-package elixir-ts-mode
  :defer t
  :mode (("\\.ex$" . elixir-ts-mode)
         ("\\.exs$" . elixir-ts-mode)
         ("\\.heex$" . heex-ts-mode))
  :init
  (add-to-list 'exec-path "~/.bin/elixir-ls")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(elixir-ts-mode "language_server.sh")))
  :hook
  (elixir-ts-mode . eglot-ensure)
  (heex-ts-mode . eglot-ensure))
