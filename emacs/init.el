;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/ (Check for any new posts)
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
          '("config/emacs-interactive-commands.el"
            "config/emacs-core.el"
            "config/package-keybindings.el"
            "config/package-manager.el"
            "config/window-configuration.el"
            "config/debug-tools.el"
            "config/git.el"
            "config/tree-sitter.el"
            "config/completion.el"
            "config/notes.el"
            "config/utils.el"
            "config/langs.el"
            "config/themes.el"
            "config/meow.el"
            ;;"config/evil.el"
            "custom.el")
        '("config/emacs-slim.el")))

(dolist (config-file-name module-file-names)
  (load (locate-user-emacs-file config-file-name)))
