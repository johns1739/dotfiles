;; https://jwiegley.github.io/use-package/keywords/
(use-package emacs
  :demand
  :init
  (setq-default
   cursor-type 'bar
   display-line-numbers 'relative
   frame-title-format '("%b")
   truncate-lines nil
   indent-tabs-mode nil
   display-fill-column-indicator-column 80)
  (setq
   confirm-kill-emacs 'y-or-n-p
   require-final-newline t
   apropos-do-all t
   create-lockfiles nil
   global-auto-revert-non-file-buffers t
   inhibit-startup-message t
   initial-scratch-message ""
   make-backup-files nil
   read-process-output-max (* 1024 1024) ;; 1mb
   ring-bell-function 'ignore
   vc-follow-symlinks t)
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (set-face-font 'default "-*-Hack Nerd Font-normal-normal-normal-*-13-*-*-*-p-0-iso10646-1")
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 120))
  (fido-vertical-mode 1)
  (delete-selection-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-auto-revert-mode t)
  (menu-bar-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (tool-bar-mode -1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package general
  :demand
  :config
  (general-define-key
   "<escape>" '(keyboard-escape-quit :wk "Quit")
   "M-/" '(hippie-expand :wk "Autocomplete"))

  (general-create-definer my/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC")

  (my/leader-key-def 'normal 'override
    "." '(find-file :wk "Find file")

    "f" '(:ignore t :wk "File")

    "e" '(:ignore t :wk "Emacs")
    "e e" 'eval-last-sexp
    "e E" 'eval-region
    "e x" 'eval-expression
    "e X" 'eval-buffer
    "e c" 'eval-buffer
    "e c" '(my/go-to-plugins-file :wk "Config")
    "e C" '(my/reload-init :wk "Reload config")

    "c" '(:ignore t :wk "Code")
    "c g" '(my/go-to-plugins-file :wk "Open plugins config")
    "c G" '(my/reload-init :wk "Reload init")
    "c c" '(compile :wk "Compile")
    "c r" '(recompile :wk "Recompile")
    "c f" '(lsp-format-buffer :wk "Format code")

    "h" '(:ignore t :wk "Help")
    "h v" 'describe-variable
    "h f" 'describe-function
    "h k" 'describe-key
    "h a" 'apropos-documentation
    "h m" 'describe-mode
    "h x" 'describe-command))


(use-package evil
  :demand
  :init
  ;; https://evil.readthedocs.io/en/latest/settings.html?highlight=evil-want#settings
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-search-module 'evil-search)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (setq evil-motion-state-cursor 'hollow)
  (setq evil-visual-state-cursor 'hollow)
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand
  :after evil
  :config
  (evil-collection-init))

(use-package lsp-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (my/leader-key-def 'normal 'override
    :keymaps 'lsp-mode-map
    "l" '(:ignore t :wk "LSP")
    "l d" 'lsp-find-definition
    "l r" 'lsp-find-references
    "l f" 'lsp-format-buffer)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package company
  :init
  (general-define-key
   :keymaps 'company-active-map
   "TAB" 'company-complete-common-or-cycle
   "C-l" 'company-complete-selection
   "RET" nil ;; terminal mode
   "<return>" nil) ;; window mode
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  :commands company-mode
  :config
  (global-company-mode 1))

(use-package flycheck
  :init
  (my/leader-key-def 'normal 'override
    :keymaps 'flycheck-mode-map
    "x" '(:ignore t :wk "Flycheck")
    "x t" '(flycheck-mode :wk "Toggle flycheck")
    "x T" '(global-flycheck-mode :wk "Toggle global flycheck")
    "x l" '(flycheck-list-errors :wk "List errors"))
  :commands flycheck-mode
  :config
  (global-flycheck-mode))

(use-package ruby-mode
  :hook
  (ruby-mode . lsp-deferred)
  (ruby-mode . display-fill-column-indicator-mode))

(use-package yaml-mode)

(use-package elixir-mode
  :init
  (setq lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.15.1/elixir-ls-v0.15.1.zip")
  :hook
  (elixir-mode . lsp-deferred))

(use-package zig-mode
  :init
  (setq lsp-zig-zls-executable "/Users/juanbanda/workspace/zls/zig-out/bin/zls")
  :hook
  (zig-mode . lsp-deferred))

(use-package projectile
  :init
  (defun my/copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (kill-new
     (file-relative-name (buffer-file-name) (projectile-project-root))))
  (setq projectile-completion-system 'default
        projectile-project-search-path '("~/workspace/")
        projectile-sort-order 'recently-active)
  (my/leader-key-def 'normal 'override
    ;; "p" 'projectile-command-map
    "SPC" '(projectile-find-file :wk "Find project file")
    "f ." '(dired-jump :wk "Dired")
    "f f" '(find-file :wk "Find file")
    "f d" '(projectile-find-dir :wk "Find project dir")
    "f r" '(projectile-recentf :wk "Find recent file")
    "f i" '(imenu :wk "Imenu")
    "f s" '(isearch-forward-word :wk "Isearch word")
    "f g" '(projectile-ripgrep :wk "Word search")
    "f t" '(projectile-find-test-file :wk "Find test")
    "f p" '(projectile-switch-project :wk "Switch project")
    "f b" '(projectile-switch-to-buffer :wk "Find buffer")
    "f y" '(my/copy-relative-file-name :wk "Copy relative filename")
    "f B" '(projectile-ibuffer :wk "Ibuffer"))
  :config
  (projectile-mode 1))

(use-package ripgrep)

(use-package which-key
  :init
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay nil)
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
  :init
  (setq doom-themes-enable-bold nil)
  (setq doom-themes-enable-italic nil))

(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  (setq doom-modeline-display-misc-in-all-mode-lines nil)
  (setq doom-modeline-env-version nil)
  :config
  (doom-modeline-mode 1))

(use-package xclip
  :unless window-system
  :config
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
  :config
  (load-theme 'gruvbox-dark-hard t))
