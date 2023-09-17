;; TODO Keybindgs of org / modes to 'm'
;; TODO Use eglot? https://github.com/joaotavora/eglot
;; TODO Move all keybinds to one area
;; TODO general
;; https://www.youtube.com/watch?v=fnE0lXoe7Y0
;; https://github.com/noctuid/general.el#use-package-keywords
;; https://github.com/jwiegley/use-package/issues/679

;; https://jwiegley.github.io/use-package/keywords/
(use-package emacs
  :demand
  :init
  (setq-default
   cursor-type 'bar
   frame-title-format '("%b")
   truncate-lines nil
   indent-tabs-mode nil
   display-line-numbers-type 'relative
   display-fill-column-indicator-column 90)
  :custom
  (apropos-do-all t)
  (completion-cycle-threshold 3)
  (confirm-kill-emacs 'y-or-n-p)
  (create-lockfiles nil)
  (global-auto-revert-non-file-buffers t)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (make-backup-files nil)
  (read-process-output-max (* 1024 1024))
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (tab-always-indent 'complete)
  (use-dialog-box nil)
  (vc-follow-symlinks t)
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (set-face-font 'default "-*-Hack Nerd Font-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")
  ;; (set-face-attribute 'default nil :height 200)
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 120))
  ;; (fido-vertical-mode nil) ;; replaced by vertico
  (delete-selection-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-auto-revert-mode t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (tool-bar-mode -1))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package saveplace
  :init
  (save-place-mode 1))

(use-package recentf
  :init
  (recentf-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package dabbrev
  :bind
  (("M-/" . dabbrev-expand)
   ("C-M-/" . dabbrev-completion)))

(use-package general)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-i-jump nil)
  (setq evil-shift-width 2)
  (setq evil-search-module 'evil-search)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (setq evil-motion-state-cursor 'hollow)
  (setq evil-visual-state-cursor 'hollow)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t)
  :config
  ;; https://evil.readthedocs.io/en/latest/settings.html?highlight=evil-want#settings
  (evil-mode 1))

(use-package evil-collection
  :demand
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; NOTE lsp formatting for solargraph does not work
(use-package eglot
  :disabled)

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package ruby-mode
  :hook
  (ruby-mode . lsp-deferred)
  (ruby-mode . display-line-numbers-mode)
  (ruby-mode . display-fill-column-indicator-mode))

(use-package yaml-mode
  :hook
  (yaml-mode . display-line-numbers-mode))

(use-package elixir-mode
  :hook
  (elixir-mode . lsp-deferred)
  (elixir-mode . display-line-numbers-mode))

(use-package project
  :init
  (defun my/project-copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (kill-new
     (file-relative-name (buffer-file-name) (project-root (project-current t))))))

(use-package rg)

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay nil)
  :config
  (which-key-mode))

(use-package magit)

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-display-misc-in-all-mode-lines nil)
  (doom-modeline-env-version nil)
  :init
  (doom-modeline-mode 1))

(use-package xclip
  :unless (display-graphic-p)
  :init
  (xclip-mode 1))

(use-package tree-sitter
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package git-link
  :after magit)

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-hard t))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode 1))

(use-package vertico-reverse
  :disabled
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :init
  (vertico-reverse-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode 1))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package vterm
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000))

(use-package org
  :mode (("\\.org$" . org-mode)))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/workspace/notes/org"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))
