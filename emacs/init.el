;;-*- lexical-binding: t; -*-

;; TODO:

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(dolist (config-file-name
         '("config/emacs-defaults.el"
           "config/emacs-commands.el"
           "config/org.el"
           "config/tree-sitter.el"
           "config/package-manager.el"
           "config/package-utils.el"
           "config/package-langs.el"
           ;; "config/package-evil.el"
           "config/package-themes.el"
           "config/keybindings.el"
           "custom.el"))
  (load (locate-user-emacs-file config-file-name)))
