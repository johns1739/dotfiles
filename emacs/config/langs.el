;; TODO: Attempt to re-introduce eglot for better integrated experience
;; Eglot is slow to update diagnostics
;; Eglot sometimes causes lag
;; Unable to turn on ruby-formatting in Eglot

(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format-buffer)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)


;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 12)
  (lsp-eldoc-render-all t)
  :init
  (defun corfu-lsp-setup ()
    (setq completion-styles '(basic orderless)
          completion-category-defaults nil))
  (defun lsp-set-bindings ()
    "Inject lsp bindings."
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . lsp-format-buffer)
               ([remap evil-lookup] . lsp-describe-thing-at-point)
               ([remap eldoc] . lsp-describe-thing-at-point)
               ([remap xref-find-references] . lsp-find-references)
               ([remap xref-find-definitions] . lsp-find-definition)
               ([remap evil-goto-definition] . lsp-find-definition)))
  :hook
  (lsp-managed-mode . corfu-lsp-setup)
  (lsp-managed-mode . lsp-set-bindings)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]erl_crash.dump\\'"))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :init
  (defun lsp-go-save-hooks ()
    (setq tab-width 4)
    (setq go-ts-mode-indent-offset 4)
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
  :defer t
  :hook
  (elm-mode . eglot-ensure))

(use-package ruby-ts-mode
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (defun ruby-set-outline-regexp ()
    (setq outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)"))
  (defun ruby-set-bindings ()
    "Inject ruby specific keybindings"
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . rails-compile)
               ([remap comint] . rails-comint)))
  (with-eval-after-load 'compile
    (push 'minitest-test compilation-error-regexp-alist)
    (push '(minitest-test "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]"
                          1 ;; file
                          2 ;; line
                          nil ;; col
                          nil ;; type
                          1 ;;hyperlink
                          )
          compilation-error-regexp-alist-alist))
  :hook
  (ruby-ts-mode . ruby-set-bindings)
  (ruby-ts-mode . ruby-set-outline-regexp)
  (ruby-ts-mode . display-fill-column-indicator-mode)
  (ruby-ts-mode . lsp-deferred))

(use-package erlang
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
  (require 'erlang-start))

;; TODO: Update compilation-error-regexp-alist-alist to match elixir file names
(use-package elixir-ts-mode
  :defer t
  :init
  (add-to-list 'exec-path "~/.bin/elixir-ls")
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                '(elixir-ts-mode "language_server.sh")))
  :custom
  (lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.20.0/elixir-ls-v0.20.0.zip")
  (lsp-elixir-suggest-specs nil)
  (lsp-elixir-enable-test-lenses nil)
  :hook
  ;; (elixir-ts-mode . eglot-ensure)
  ;; (heex-ts-mode . eglot-ensure))
  (elixir-ts-mode . lsp-deferred)
  (heex-ts-mode . lsp-deferred))

(use-package lsp-haskell
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
