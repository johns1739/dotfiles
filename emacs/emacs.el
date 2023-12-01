;; -*- lexical-binding: t; -*-

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)
            (setq gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))


;;;; SETTINGS

(setq apropos-do-all t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq create-lockfiles nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)
(setq global-auto-revert-non-file-buffers t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(setq read-process-output-max (* 1024 1024))
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq tab-always-indent 'complete)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)
(setq-default display-fill-column-indicator-column 90)
(setq-default display-line-numbers-type 'relative)
(setq-default frame-title-format '("%b"))
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))
(delete-selection-mode 1)
(electric-pair-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(set-face-font 'default "-*-Hack Nerd Font-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")
(show-paren-mode 1)
(tool-bar-mode -1)
;; (auto-save-visited-mode 1)

;; To install grammars:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(with-eval-after-load 'treesit
 (add-to-list 'treesit-language-source-alist '(heex "https://github.com/phoenixframework/tree-sitter-heex"))
 (add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir")))


;;;; DEFINITIONS

(defun my/project-copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new
   (file-relative-name (buffer-file-name) (project-root (project-current t)))))

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

(defun my/go-to-emacs-file ()
  "Go to my emacs file."
  (interactive)
  (find-file my/emacs-file-name))

(defun my/go-to-packages-file ()
  "Go to my packages file."
  (interactive)
  (find-file my/packages-file-name))

(defun my/go-to-keymaps-file ()
  "Go to my keymaps file."
  (interactive)
  (find-file my/keymaps-file-name))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
