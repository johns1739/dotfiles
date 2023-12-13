;;-*- lexical-binding: t; -*-

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)
            (setq gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(defconst my/config-directory "~/workspace/myconfigs/emacs")
(defconst my/init-file-name (expand-file-name "init.el" my/config-directory))
(defconst my/emacs-file-name (expand-file-name "emacs.el" my/config-directory))
(defconst my/packages-file-name (expand-file-name "packages.el" my/config-directory))
(defconst my/keymaps-file-name (expand-file-name "keymaps.el" my/config-directory))

(load-file my/emacs-file-name)
(load-file my/packages-file-name)
(load-file my/keymaps-file-name)
