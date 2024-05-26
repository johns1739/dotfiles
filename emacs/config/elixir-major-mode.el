(use-package elixir-ts-mode
  :defer t
  :init
  (add-to-list 'exec-path "~/.bin/elixir-ls")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(elixir-ts-mode "language_server.sh")))
  :custom
  (lsp-elixir-ls-download-url
   "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.20.0/elixir-ls-v0.20.0.zip")
  (lsp-elixir-suggest-specs nil)
  (lsp-elixir-enable-test-lenses nil)
  :hook
  (elixir-ts-mode . lsp-ensure-caller)
  (heex-ts-mode . lsp-ensure-caller))
