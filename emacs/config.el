(package-initialize)

;; TODO: / search cannot paste into it
;; LSP diagnostics sometimes slow?
;; TODO: Parens match better highlight
;; TODO: Live search

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(use-package emacs
  :config

  (setq-default
   cursor-type 'bar
   display-line-numbers 'relative
   frame-title-format '("%b")
   truncate-lines nil
   indent-tabs-mode nil
   display-fill-column-indicator-column 100)

  (setq
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

(use-package general
  :config
  (general-define-key
   "<escape>" '(keyboard-escape-quit :wk "Quit")
   "M-/" '(hippie-expand :wk "Autocomplete"))

  (general-create-definer rune/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC")

  (rune/leader-key-def 'normal 'override
    "." '(find-file :wk "Find file")

    "c" '(:ignore t :wk "Config")
    "c c" '(my/go-to-config-file :wk "Open config")
    "c r" '(my/reload-configs :wk "Reload config")

    "f" '(:ignore t :wk "File")
    "h" '(:ignore t :wk "Help")
    "h v" 'describe-variable
    "h f" 'describe-function
    "h k" 'describe-key
    "h d" 'apropos-documentation
    "h x" 'describe-command))


(use-package evil
  :demand t
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
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package lsp-mode
  :init
  ;; (setq lsp-keymap-prefix "M-l")
  ;; TODO: Bind importang commands
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package company
  ;; TODO: Unset enter for autocomplete
  :init
  (global-company-mode))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package ruby-mode
  :hook
  (ruby-mode . lsp-deferred))
  ;; (ruby-mode . display-fill-column-indicator-mode))

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package projectile
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-project-search-path '("~/workspace/"))
  (projectile-mode 1)
  (rune/leader-key-def 'normal 'override
    ;; "p" 'projectile-command-map
    "SPC" '(projectile-find-file :wk "Find project file")
    "f ." '(projectile-find-file-in-directory :wk "Find project cwd file")
    "f f" '(find-file :wk "Find file")
    "f d" '(projectile-find-dir :wk "Find project dir")
    "f r" '(projectile-recentf :wk "Find recent file")
    "f s" '(projectile-ripgrep :wk "Word search")
    "f p" '(projectile-switch-project :wk "Switch project")
    "f b" '(projectile-switch-to-buffer :wk "Find buffer")
    "f B" '(projectile-ibuffer :wk "Ibuffer")))

(use-package ripgrep)

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :config
  (rune/leader-key-def 'normal 'override
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Git status")))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil)
  (setq doom-themes-enable-italic nil)
  (load-theme 'doom-rouge t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))
