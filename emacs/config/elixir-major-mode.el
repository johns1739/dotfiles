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
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
    (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]erl_crash.dump\\'"))
  :custom
  (lsp-elixir-ls-download-url
   "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.20.0/elixir-ls-v0.20.0.zip")
  (lsp-elixir-suggest-specs nil)
  (lsp-elixir-enable-test-lenses nil)
  :hook
  (elixir-ts-mode . lsp-ensure-caller)
  (heex-ts-mode . lsp-ensure-caller))
