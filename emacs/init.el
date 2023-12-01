;;-*- lexical-binding: t; -*-

(defconst my/config-directory "~/workspace/myconfigs/emacs")
(defconst my/init-file-name (expand-file-name "init.el" my/config-directory))
(defconst my/emacs-file-name (expand-file-name "emacs.el" my/config-directory))
(defconst my/packages-file-name (expand-file-name "packages.el" my/config-directory))
(defconst my/keymaps-file-name (expand-file-name "keymaps.el" my/config-directory))

(load-file my/emacs-file-name)
(load-file my/packages-file-name)
(load-file my/keymaps-file-name)
