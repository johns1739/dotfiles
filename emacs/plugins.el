;; TODO org-mode

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
   (completion-cycle-threshold 3)
   (tab-always-indent 'complete)
   (confirm-kill-emacs 'y-or-n-p)
   (require-final-newline t)
   (apropos-do-all t)
   (create-lockfiles nil)
   (global-auto-revert-non-file-buffers t)
   (inhibit-startup-message t)
   (initial-scratch-message "")
   (make-backup-files nil)
   (read-process-output-max (* 1024 1024))
   (ring-bell-function 'ignore)
   (vc-follow-symlinks t)
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (set-face-font 'default "-*-Hack Nerd Font-normal-normal-normal-*-13-*-*-*-p-0-iso10646-1")
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
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package general
  :demand
  :config
  (general-define-key
   "<escape>" '(keyboard-escape-quit :wk "Quit"))

  (general-create-definer my/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC")

  (my/leader-key-def 'normal 'override
    "." 'find-file
    ">" '(dired-jump :wk "Dired")
    "SPC" 'project-find-file

    "e" '(:ignore t :wk "Emacs")
    "e c" '(my/go-to-plugins-file :wk "Config")
    "e C" '(my/reload-init :wk "Reload config")

    "o" '(:ignore t :wk "Open")
    "o c" '(my/go-to-plugins-file :wk "Open config")
    "o C" '(my/reload-init :wk "Reload init")

    "c" '(:ignore t :wk "Code")
    "c c" '(compile :wk "Compile")
    "c r" '(recompile :wk "Recompile")))

(use-package evil
  :custom
  ;; https://evil.readthedocs.io/en/latest/settings.html?highlight=evil-want#settings
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2)
  (evil-search-module 'evil-search)
  (evil-insert-state-cursor 'bar)
  (evil-normal-state-cursor 'box)
  (evil-motion-state-cursor 'hollow)
  (evil-visual-state-cursor 'hollow)
  (which-key-allow-evil-operators t)
  (which-key-show-operator-state-maps t)
  :init
  (evil-mode 1))

(use-package evil-collection
  :demand
  :after evil
  :config
  (evil-collection-init))

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
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
  ;; (ruby-mode . display-line-numbers-mode)
  (ruby-mode . display-fill-column-indicator-mode))

(use-package yaml-mode
  :hook
  (yaml-mode . display-line-numbers-mode))

(use-package elixir-mode
  :init
  (setq lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.15.1/elixir-ls-v0.15.1.zip")
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
    "p D" 'project-dired
    "p f" 'project-find-file
    "p p" 'project-switch-project
    "p R" 'project-query-replace-regexp
    "p g" 'project-find-regexp
    "p y" 'my/project-copy-relative-file-name))

(use-package ripgrep)

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

;; (use-package treemacs
;;   :init
;;   (setq treemacs-no-png-images t)
;;   (my/leader-key-def 'normal 'override
;;     "f e" '(treemacs :wk "Treemacs"))
;;   :config
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-fringe-indicator-mode 'always))

;; (use-package treemacs-evil
;;   :after treemacs)

;; (use-package treemacs-projectile
;;   :after treemacs)

;; (use-package treemacs-magit
;;   :after (treemacs magit))

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
    "f /" 'consult-line
    "f ?" 'consult-line-multi
    "f B" 'consult-buffer
    "f I" 'consult-imenu-multi
    "f b" 'consult-project-buffer
    "f f" 'consult-find
    "f i" 'consult-imenu
    "f r" 'consult-recent-file
    "f g" 'consult-ripgrep))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (general-define-key
   :keymaps 'corfu-map
   "C-SPC" 'corfu-insert-separator
   "RET" nil
   "<return>" nil)
  (global-corfu-mode 1))

(use-package vterm
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000)
  :init
  (my/leader-key-def 'normal 'override
    "o t" 'vterm-other-window
    "o T" 'vterm))
