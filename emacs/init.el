;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/
;; TODO: Attempt to re-introduce eglot for better integrated experience
;; Eglot is slow to update diagnostics
;; Eglot sometimes causes lag
;; Unable to turn on ruby-formatting in Eglot
;; TODO: Create repeat keymaps for those with prev/next bindings
;; TODO: Review meow tutorials
;; TODO: Review popper tutorials

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(setq module-file-names
      (if (display-graphic-p)
          '("config/emacs-core.el"
            "config/package-manager.el"
            "config/package-core.el"
            "config/lsp.el"
            "config/goto.el"
            "config/search.el"
            "config/completion.el"
            "config/diagnostics.el"
            "config/git.el"
            "config/notes.el"
            "config/toggle.el"
            "config/ruby-major-mode.el"
            "config/elixir-major-mode.el"
            "config/major-modes.el"
            "config/debug-tools.el"
            "config/themes.el"
            "config/meow.el"
            ;;"config/evil.el"
            "custom.el")
        '("config/emacs-core.el"
          "config/ruby-major-mode.el"
          "config/elixir-major-mode.el")))

(dolist (config-file-name module-file-names)
  (load (locate-user-emacs-file config-file-name)))
