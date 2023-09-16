;; TODO emacs flymake config
;; TODO treemacs
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
   display-fill-column-indicator-column 85)
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

(use-package general
  :demand
  :config
  (general-define-key
   "M-j" '(evil-avy-goto-char-2 :wk "Jump to char")
   "<escape>" '(keyboard-escape-quit :wk "Quit"))

  (general-create-definer my/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC")

  (my/leader-key-def '(normal visual) 'override
    "." 'find-file
    ">" '(dired-jump :wk "Dired")
    "SPC" 'project-find-file

    "s" '(:ignore t :wk "Search")
    "s s" 'consult-line
    "s i" 'consult-imenu

    "e" '(:ignore t :wk "Emacs")
    "e e" 'eval-last-sexp
    "e E" 'eval-defun
    "e c" '(my/go-to-plugins-file :wk "Config")
    "e C" '(my/reload-init :wk "Reload config")

    "o" '(:ignore t :wk "Open")

    "c" '(:ignore t :wk "Code")
    "c c" '(my/compile :wk "Compile")
    "c r" '(recompile :wk "Recompile")))

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

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless basic))) ;; Configure orderless
  (my/leader-key-def 'normal 'override
    :keymaps 'lsp-mode-map
    "l" '(:ignore t :wk "LSP")
    "l d" 'lsp-find-definition
    "l r" 'lsp-find-references
    "l f" 'lsp-format-buffer)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands (lsp lsp-deferred))

(use-package flycheck
  :init
  (my/leader-key-def 'normal 'override
    :keymaps 'flycheck-mode-map
    "k" '(:ignore t :wk "Flycheck")
    "k t" '(flycheck-mode :wk "Toggle flycheck")
    "k T" '(global-flycheck-mode :wk "Toggle global flycheck")
    "k l" '(flycheck-list-errors :wk "List errors"))
  :commands flycheck-mode
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
  :init
  (setq lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.15.1/elixir-ls-v0.15.1.zip")
  (setq lsp-elixir-suggest-specs nil)
  :hook
  (elixir-mode . display-line-numbers-mode)
  (elixir-mode . lsp-deferred))

(use-package zig-mode
  :init
  (setq lsp-zig-zls-executable "/Users/juanbanda/workspace/zls/zig-out/bin/zls")
  :hook
  (zig-mode . display-line-numbers-mode)
  (zig-mode . lsp-deferred))

(use-package project
  :init
  (defun my/project-copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (kill-new
     (file-relative-name (buffer-file-name) (project-root (project-current t)))))

  (my/leader-key-def 'normal 'override
    "p" '(:ignore t :wk "Project")
    "p !" 'project-shell-command
    "p &" 'project-async-shell-command
    "p b" 'project-switch-to-buffer
    "p B" 'project-list-buffers
    "p c" 'project-compile
    "p d" 'project-find-dir
    "p ." 'project-dired
    "p D" 'project-forget-project
    "p f" 'project-find-file
    "p p" 'project-switch-project
    "p R" 'project-query-replace-regexp
    ;; "p g" 'project-find-regexp ;; opens up in xref-mode which prevents wgrep
    "p g" 'rg-project
    "p G" 'rg-dwim
    "p y" 'my/project-copy-relative-file-name))

(use-package rg)

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay nil)
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package magit
  :init
  (my/leader-key-def 'normal 'override
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Git status")
    "g G" '(magit-file-dispatch :wk "Git dispatch buffer")
    "g B" '(magit-blame-addition :wk "Show blame")
    "g l" '(magit-log-buffer-file :wk "Git logs")
    "g d" '(magit-diff-buffer-file :wk "Git diff")
    "g b" '(magit-blame :wk "Git blame")))

(use-package doom-themes
  :disabled
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

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
  :after magit
  :init
  (my/leader-key-def '(normal visual) 'override
    "g y" '(git-link :wk "Git link")))

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

(use-package consult
  :init
  (my/leader-key-def 'normal 'override
    "f" '(:ignore t :wk "Fuzzy Search")
    "f l" 'consult-line-multi
    "f B" 'consult-buffer
    "f i" 'consult-imenu-multi
    "f b" 'consult-project-buffer
    "f f" 'consult-find
    "f r" 'consult-recent-file
    "f s" 'consult-ripgrep))

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
  (general-define-key
   :keymaps 'corfu-map
   "C-SPC" 'corfu-insert-separator
   "RET" nil
   "<return>" nil)
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
  (vterm-max-scrollback 100000)
  :init
  (my/leader-key-def 'normal 'override
    "o t" 'vterm-other-window
    "o T" 'vterm))

(use-package avy)

(use-package wgrep)

(use-package org
  :mode (("\\.org$" . org-mode)))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :init
  (my/leader-key-def 'normal 'override
    "n" '(:ignore t :wk "Org Notes")
    "n l" 'org-roam-buffer-toggle
    "n f" 'org-roam-node-find
    "n g" 'org-roam-graph
    "n i" 'org-roam-node-insert
    "n j" 'org-roam-dailies-capture-today
    "n c" 'org-roam-capture)
  :custom
  (org-roam-directory (file-truename "~/workspace/notes/org"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))
