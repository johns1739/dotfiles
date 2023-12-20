;; -*- lexical-binding: t; -*-

;; This contains builtin emacs logic.
;; Does not depend on any third party packages.

;;;; SETTINGS

(setq apropos-do-all t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq create-lockfiles nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq eldoc-echo-area-use-multiline-p nil)
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
(column-number-mode 1)
(delete-selection-mode 1)
(desktop-save-mode -1)
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
(window-divider-mode 1)
;; (auto-save-visited-mode 1)

;; To install grammars:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby"))))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))


;; COMMANDS

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

(defun my/project-directory ()
  "Current project directory."
  (project-root (project-current t)))

(defun my/project-relative-file-name ()
  "Relative project path to file."
  (file-relative-name (buffer-file-name) (my/project-directory)))

(defun my/project-copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (my/project-relative-file-name)))

(defun my/rails-buffer-test-file-p ()
  (string-match-p "_test.rb\\'" (buffer-file-name)))

(defun my/rails-test-file-compile-command ()
  (let ((linum (number-to-string (line-number-at-pos)))
        (file-name (my/project-relative-file-name)))
    (string-join (list "rails t " (s-concat file-name ":" linum) " "))))

(defun my/rails-dwim-compile-command ()
  (cond ((my/rails-buffer-test-file-p) (my/rails-test-file-compile-command))
        (t compile-command)))

(defun my/rails-compile ()
  (interactive)
  (let ((compile-command (my/rails-dwim-compile-command)))
    (call-interactively #'project-compile)))

(defun my/rails-compile-comint ()
  "Dwim compilation for ruby files."
  (interactive)
  (universal-argument)
  (command-execute 'my/rails-compile))
