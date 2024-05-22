;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/ (Check for any new posts)
;; TODO: Master paraedit

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(dolist (config-file-name
         '("config/emacs-defaults.el"
           "config/emacs-interactive-commands.el"
           "config/keybindings.el"
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
           "custom.el"))
  (load (locate-user-emacs-file config-file-name)))
