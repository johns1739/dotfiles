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

;; My definitions

(defconst my/config-directory "~/workspace/myconfigs/emacs")

(defconst my/init-file-name
  (expand-file-name "init.el" my/config-directory))

(defconst my/packages-file-name
  (expand-file-name "packages.el" my/config-directory))

(defconst my/keymaps-file-name
  (expand-file-name "keymaps.el" my/config-directory))

(defun my/compile (command)
  "Compile COMMAND using region or from prompt."
  (interactive
   (list
    (read-string "Compile command: "
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))))))
  (compile command))

(defun my/reload-init ()
  "Reload my emacs configuration."
  (interactive)
  (load-file my/init-file-name))

(defun my/go-to-init-file ()
  "Go to my init file."
  (interactive)
  (find-file my/init-file-name))

(defun my/go-to-packages-file ()
  "Go to my packages file."
  (interactive)
  (find-file my/packages-file-name))

(defun my/go-to-keymaps-file ()
  "Go to my keymaps file."
  (interactive)
  (find-file my/keymaps-file-name))

;; Install and configure the straight package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-compute-statistics t)

;; Install packages
(load-file my/packages-file-name)
(load-file my/keymaps-file-name)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
