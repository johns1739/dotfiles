;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/
;; TODO: Attempt to re-introduce eglot for better integrated experience
;; Eglot is slow to update diagnostics
;; Eglot sometimes causes lag
;; Unable to turn on ruby-formatting in Eglot
;; TODO: Review meow tutorials
;; TODO: Review popper tutorials
;; Recompile as comint from compile
;; Ability rerun comint

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq module-file-names
      '("config/emacs.el"

        ;; Package setup
        "config/straight.el"
        "config/package-core.el"

        ;; Essentials
        "config/goto.el"
        "config/search.el"
        "config/completion.el"
        "config/compilation.el"
        "config/diagnostics.el"
        "config/git.el"
        "config/notes.el"
        "config/toggle.el"

        ;; Languages
        "config/lsp.el"
        "config/ruby-major-mode.el"
        "config/elixir-major-mode.el"
        "config/major-modes.el"

        ;; Utils
        "config/debug-tools.el"
        "config/themes.el"
        "config/meow.el"
        "custom.el")
      )

(dolist (config-file-name module-file-names)
  (load (locate-user-emacs-file config-file-name)))
