;;-*- lexical-binding: t; -*-

;; TODO
;; Send bash region to vterm from other buffer

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(defconst my/config-file-name (expand-file-name "init.el" user-emacs-directory))
(defconst my/yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))

(setq apropos-do-all t)
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq create-lockfiles nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq eldoc-echo-area-use-multiline-p nil)
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 16 1000 1000)) ;; 16 MB
(setq global-auto-revert-non-file-buffers t)
(setq hippie-expand-verbose t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(setq read-process-output-max (* 1024 1024))
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq tab-always-indent 'complete)
(setq use-dialog-box nil)
(setq history-length 500)
(setq history-delete-duplicates t)
(setq-default cursor-type 'bar)
(setq-default display-fill-column-indicator-column 90)
(setq-default display-line-numbers-type 'relative)
(setq-default fill-column 120)
(setq-default frame-title-format '("%b"))
(setq-default indent-tabs-mode nil)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-line
        try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-file-name-partially
        try-complete-lisp-symbol
        try-complete-file-name
        try-expand-dabbrev-from-kill))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))
(column-number-mode 1)
(delete-selection-mode 1)
(electric-indent-mode 1)
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

;; (desktop-save-mode -1)
;; (auto-save-visited-mode 1)

;; Install grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (haskell "https://github.com/tree-sitter/haskell-tree-sitter")
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
          (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (sql "https://github.com/DerekStride/tree-sitter-sql"))))

;; COMMANDS

(defun my/reload-config ()
  "Reload my emacs configuration."
  (interactive)
  (load-file my/config-file-name))

(defun my/go-to-config-file ()
  "Go to my config file."
  (interactive)
  (find-file my/config-file-name))

(defun my/project-copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (my/project-relative-file-name)))

(defun my/rails-compile ()
  (interactive)
  (setq compile-command (my/rails-dwim-compile-command))
  (call-interactively #'project-compile))

(defun my/rails-compile-comint ()
  "Dwim compilation for ruby files."
  (interactive)
  (universal-argument)
  (command-execute 'my/rails-compile))


;;;; HELPERS

(defun my/project-directory ()
  "Current project directory."
  (project-root (project-current t)))

(defun my/project-relative-file-name ()
  "Relative project path to file."
  (file-relative-name (buffer-file-name) (my/project-directory)))

(defun my/project-expand (file-name)
  (f-join (my/project-directory) file-name))

(defun my/rails-buffer-test-file-p ()
  (string-match-p "_test.rb\\'" (buffer-file-name)))

(defun my/rails-buffer-ruby-file-p ()
  (string-match-p ".rb\\'" (buffer-file-name)))

(defun my/rails-test-file-compile-command ()
  (let ((linum (number-to-string (line-number-at-pos)))
        (file-name (my/project-relative-file-name)))
    (if (< (line-number-at-pos) 5)
        (string-join (list "rails t " file-name))
      (string-join (list "rails t " (s-concat file-name ":" linum))))))

(defun my/rails-test-file-exists-p ()
  (and (my/rails-buffer-ruby-file-p)
       (file-exists-p (my/project-expand (my/rails-generate-test-file-name)))))

(defun my/rails-generate-test-file-name ()
  (let* ((file-name (my/project-relative-file-name))
         (test-file-name (concat (f-base file-name) "_test.rb"))
         (folder-split (cdr (s-split "/" (f-dirname file-name)))))
    (f-join "test" (apply 'f-join  folder-split) test-file-name)))

(defun my/rails-dwim-compile-command ()
  (cond ((my/rails-buffer-test-file-p)
         (my/rails-test-file-compile-command))
        (t compile-command)))


;; INSTALL PACKAGE MANAGER
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)


;; GLOBAL KEYMAP

(keymap-global-set "C-x h" '("Previous buffer" . previous-buffer))
(keymap-global-set "C-x l" '("Next buffer" . next-buffer))
(keymap-global-set "M-/" 'hippie-expand)


;;;; PACKAGES

(use-package evil
  :demand t
  :init
  (defun my/set-eglot-bindings ()
    "Inject eglot bindings."
    (keymap-set evil-normal-state-local-map "g = =" 'eglot-format-buffer)
    (keymap-set evil-normal-state-local-map "g R" 'eglot-rename))
  (defun my/set-lsp-bindings ()
    "Inject lsp bindings."
    (keymap-set evil-normal-state-local-map "g = =" 'lsp-format-buffer)
    (keymap-set evil-normal-state-local-map "g r" 'lsp-find-references)
    (keymap-set evil-normal-state-local-map "g = r" 'lsp-format-region)
    (keymap-set evil-normal-state-local-map "g R" 'lsp-rename)
    (keymap-set evil-normal-state-local-map "g d" 'lsp-find-definition)
    (keymap-set evil-normal-state-local-map "K" 'eldoc))
  (defun my/set-ruby-bindings ()
    "Inject ruby specific keybindings"
    (keymap-set evil-normal-state-local-map "SPC c c" 'my/rails-compile)
    (keymap-set evil-normal-state-local-map "SPC c C" 'my/rails-compile-comint))
  (setq evil-disable-insert-state-bindings t)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-motion-state-cursor 'hollow)
  (setq evil-normal-state-cursor 'box)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind (:map evil-normal-state-map
              ("SPC SPC" . project-find-file)
              ("SPC e I" . my/reload-config)
              ("SPC e i" . my/go-to-config-file)
              ("SPC e R" . restart-emacs)
              ("SPC s g" . occur)
              ("SPC f p" . project-switch-project)
              ("SPC f d" . project-find-dir)
              ("SPC f g" . project-find-regexp)
              ("SPC c C" . project-compile)
              ("SPC c c" . compile)
              ("SPC c r" . recompile)
              ("SPC c y" . my/project-copy-relative-file-name))
  :hook
  (eglot-managed-mode . my/set-eglot-bindings)
  (lsp-managed-mode . my/set-lsp-bindings)
  (ruby-ts-mode . my/set-ruby-bindings)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ansi-color
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (let ((undo-tree-history-directory (file-name-as-directory
                                      (file-name-concat user-emacs-directory "undo-tree-history"))))
    (unless (file-exists-p undo-tree-history-directory)
      (make-directory undo-tree-history-directory))
    (setq undo-tree-history-directory-alist (list (cons "."  undo-tree-history-directory))))
  (global-undo-tree-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "GOPATH"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package rg
  :defer t)

(use-package avy
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC s j" . avy-goto-char-2)))

(use-package ace-window
  :defer t
  :bind ([remap other-window] . ace-window))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC s b" . consult-bookmark)
              ("SPC s i" . consult-imenu)
              ("SPC s s" . consult-line)
              ("SPC s r" . consult-register)
              ("SPC f B" . consult-buffer)
              ("SPC f b" . consult-project-buffer)
              ("SPC f i" . consult-imenu-multi)
              ("SPC f r" . consult-recent-file)
              ("SPC f s" . consult-ripgrep)))

(use-package vterm
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC c t" . project-vterm))
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000)
  :init
  (defun project-vterm ()
    (interactive)
    (let ((default-directory (my/project-directory)))
      (call-interactively #'vterm)))

  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-at-bottom)
                 (window-height . 12)
                 (dedicated . t))))

(use-package magit
  :defer t
  :commands (magit-blame-addition
             magit-file-dispatch
             magit-blame
             magit-diff-buffer-file
             magit-status
             magit-log-buffer-file)
  :bind (:map evil-normal-state-map
              ("SPC g B" . magit-blame-addition)
              ("SPC g G" . magit-file-dispatch)
              ("SPC g b" . magit-blame)
              ("SPC g d" . magit-diff-buffer-file)
              ("SPC g g" . magit-status)
              ("SPC g l" .  magit-log-buffer-file)))

(use-package git-link
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC g y" . git-link)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :defer t
  :custom
  (flycheck-indication-mode 'right-fringe))

;; https://joaotavora.github.io/yasnippet/index.html
(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list my/yas-snippet-dir))
  (yas-global-mode 1))

;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-eldoc-render-all t)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]erl_crash.dump\\'"))

(use-package elm-mode
  :defer t
  :hook
  (elm-mode . eglot-ensure))

(use-package ruby-ts-mode
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  :hook
  (ruby-ts-mode . display-line-numbers-mode)
  (ruby-ts-mode . lsp-deferred))

(use-package erlang
  :defer t
  :straight nil
  :load-path "/opt/homebrew/lib/erlang/lib/tools-3.6/emacs/"
  :hook
  (erlang-mode . lsp-deferred)
  :mode
  (("\\.erl?$" . erlang-mode)
   ("rebar\\.config$" . erlang-mode)
   ("relx\\.config$" . erlang-mode)
   ("sys\\.config\\.src$" . erlang-mode)
   ("sys\\.config$" . erlang-mode)
   ("\\.config\\.src?$" . erlang-mode)
   ("\\.config\\.script?$" . erlang-mode)
   ("\\.hrl?$" . erlang-mode)
   ("\\.app?$" . erlang-mode)
   ("\\.app.src?$" . erlang-mode)
   ("\\Emakefile" . erlang-mode))
  :config
  (setq erlang-root-dir "/opt/homebrew/lib/erlang")
  (add-to-list 'exec-path "/opt/homebrew/lib/erlang/bin")
  (require 'erlang-start))

(use-package lsp-haskell
  :defer t
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-mode . display-line-numbers-mode)
  (haskell-cabal-mode . display-line-numbers-mode)
  (haskell-literal-mode . lsp-deferred))

(use-package elixir-ts-mode
  :defer t
  :custom
  (lsp-elixir-suggest-specs nil)
  :hook
  (elixir-ts-mode . display-line-numbers-mode)
  (heex-ts-mode . display-line-numbers-mode)
  (elixir-ts-mode . lsp-deferred)
  (heex-ts-mode . lsp-deferred))

(use-package yaml-ts-mode
  :defer t
  :mode "\\(\\.yaml\\|.yml\\)\\'"
  :hook
  (yaml-ts-mode . lsp-deferred)
  (yaml-ts-mode . display-line-numbers-mode))

(use-package sqlformat
  :commands (sqlformat)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

;; https://www.nongnu.org/geiser/index.html
;; https://www.gnu.org/software/guile/manual/guile.html
(use-package geiser-guile
  :commands (geiser-mode))

;; http://pub.gajendra.net/src/paredit-refcard.pdf
(use-package paredit
  :disabled t
  :hook
  (scheme-mode . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode))

(use-package denote
  :defer t
  :init
  (setq denote-file-type 'markdown-yaml)
  (setq denote-directory "~/.notes/denote")
  :bind
  (:map evil-normal-state-map
        ("SPC n n" . denote))
  :config
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory)))

(use-package consult-notes
  :defer t
  :bind
  (:map evil-normal-state-map
        ("SPC n f" . consult-notes)
        ("SPC n s" . consult-notes-search-in-all-notes))
  :config
  (consult-notes-denote-mode))


;;;; COMPLETION

(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  :straight (corfu :files (:defaults "extensions/*.el") :includes (corfu-echo))
  ;; Completion in region function
  ;; https://github.com/minad/corfu#key-bindings
  :custom
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-prefix 2) ; Enable auto completion
  (corfu-auto-delay 0.2) ; Enable auto completion
  (corfu-echo-delay '(1 . 0.5))
  :init
  (global-corfu-mode 1)
  (corfu-echo-mode)
  :config
  (keymap-set corfu-map "RET" nil) ;; Prevent enter from auto-completing
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil
                  corfu-auto-delay 0.5)
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
  ;; Cape provides Completion At Point Extensions
  :after corfu
  :custom
  (completion-at-point-functions
   '(cape-dabbrev
     cape-keyword
     cape-dict
     cape-file)))

;;;; GRAPHICS

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package gruvbox-theme
  :disabled t)

(use-package modus-themes
  :config
  ;; https://protesilaos.com/emacs/modus-themes
  (setq modus-vivendi-tritanopia-palette-overrides
        '((fringe unspecified)
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))
  (when (display-graphic-p)
    (add-to-list 'modus-vivendi-tritanopia-palette-overrides '(fg-main "#d0d0d0"))
    (add-to-list 'modus-vivendi-tritanopia-palette-overrides '(bg-main "#14191e")))
  (load-theme 'modus-vivendi-tritanopia t))

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


;; CUSTOM FILE
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
