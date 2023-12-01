;; -*- lexical-binding: t; -*-

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

(use-package lsp-mode
  :init
  (defun my/lsp-mode-set-default-styles ()
    (setf (alist-get 'styles
                     (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-set-default-styles)
  :commands (lsp lsp-deferred))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.config/elixir_ls/language_server.sh"))
  (add-to-list 'eglot-server-programs '(heex-ts-mode "~/.config/elixir_ls/language_server.sh")))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (let ((undo-tree-history-directory (file-name-as-directory
                   (file-name-concat user-emacs-directory "undo-tree-history"))))
    (unless (file-exists-p undo-tree-history-directory)
      (dired-create-directory undo-tree-history-directory))
    (setq undo-tree-history-directory-alist (list (cons "."  undo-tree-history-directory))))
  (global-undo-tree-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :config
  (dolist (var '(
                 "SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "GOPATH"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package rg)

(use-package avy)

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay nil)
  :config
  (which-key-mode))

(use-package xclip
  :unless (display-graphic-p)
  :init
  (xclip-mode 1))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult)

(use-package vterm
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000))


;;;; GIT

(use-package magit)

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-link
  :after magit)


;;;; EVIL

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-search-module 'evil-search)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (setq evil-motion-state-cursor 'hollow)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
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


;;;; LANGUAGES

(use-package elixir-ts-mode
  :init
  (setq lsp-elixir-suggest-specs nil)
  (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
  :hook
  (elixir-ts-mode . eglot-ensure))

(use-package heex-ts-mode
  :after elixir-ts-mode
  :hook
  (heex-ts-mode . eglot-ensure))

(use-package ruby-mode
  :hook
  (ruby-mode . lsp-deferred)
  (ruby-mode . display-line-numbers-mode)
  (ruby-mode . display-fill-column-indicator-mode))

(use-package yaml-mode
  :hook
  (yaml-mode . display-line-numbers-mode))


;;;; ORG MODE

(use-package org
  :demand t
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-directory "~/workspace/notes/org")
  (setq org-startup-indented t))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/workspace/notes/org/roam"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))


;;;; COMPLETION

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo))
  ;; Completion in region function
  ;; https://github.com/minad/corfu#key-bindings
  :custom
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-prefix 3) ; Enable auto completion
  (corfu-auto-delay 1) ; Enable auto completion
  (corfu-echo-delay '(1 . 0.5))
  :init
  (global-corfu-mode 1)
  (corfu-echo-mode)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil
                  corfu-auto-delay 0.3)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package cape
  :after corfu
  :custom
  (completion-at-point-functions
   '(cape-file
     cape-dabbrev
     cape-dict)))


;;;; GRAPHICS

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-hard t))

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