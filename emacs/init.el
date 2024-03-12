;;-*- lexical-binding: t; -*-

;; TODO:

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(dolist (config-file-name
         '("config/package-manager.el"
           "config/defaults.el"
           "config/commands.el"
           "config/git.el"
           "config/org.el"
           "config/tree-sitter.el"
           "config/completion.el"
           "config/utils.el"
           "config/langs.el"
           "config/evil.el"
           "config/themes.el"
           "config/keybindings.el"
           "custom.el"))
  (load (locate-user-emacs-file config-file-name)))
