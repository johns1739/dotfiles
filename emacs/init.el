;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "*** Emacs loaded in %s seconds with %d garbage collections."
            (emacs-init-time "%.2f")
            gcs-done)))

(defconst my/config-directory "~/workspace/myconfigs/emacs")

(defconst my/init-file-name
  (expand-file-name "init.el" my/config-directory))

(defconst my/plugins-file-name
  (expand-file-name "plugins.el" my/config-directory))

(defconst my/config-file-name
  (expand-file-name "config.el" my/config-directory))


(defun my/reload-init ()
  "Reload my init & configs."
  (interactive)
  (load-file my/init-file-name))

(defun my/go-to-plugins-file ()
  "Go to my plugins file."
  (interactive)
  (find-file my/plugins-file-name))

;; (defun my/go-to-config-file ()
;;   "Go to my config file."
;;   (interactive)
;;   (find-file my/config-file-name))

(load-file my/plugins-file-name)
;; (load-file my/config-file-name)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
