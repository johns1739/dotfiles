;; -*- lexical-binding: t; -*-

;; TODO: Search key-bindings to use region as input

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


;;;; PACKAGES

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
  :disabled
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-link
  :after magit)


;;;; EVIL

(use-package evil
  :init
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
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))


(use-package evil-collection
  :demand
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package flycheck)

(use-package command-log-mode)

;;;; LANGUAGES

(use-package lsp-mode
  :init
  (defun my/lsp-mode-set-default-styles ()
    (setf (alist-get 'styles
                     (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use corfu
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-eldoc-render-all t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-set-default-styles)
  :commands (lsp lsp-deferred)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]erl_crash.dump\\'"))

(use-package lsp-ui
  :disabled
  :after lsp-mode
  :custom
  (lsp-ui-doc-delay 1)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-delay 1)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t))

(use-package ruby-ts-mode
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  :hook
  (ruby-ts-mode . display-fill-column-indicator-mode)
  (ruby-ts-mode . display-line-numbers-mode)
  (ruby-ts-mode . lsp-deferred))


(use-package elixir-ts-mode
  :custom
  (lsp-elixir-suggest-specs nil)
  :hook
  (elixir-ts-mode . lsp-deferred)
  (heex-ts-mode . lsp-deferred))

(use-package yaml-ts-mode
  :mode "\\.yaml\\|.yml\\'"
  :hook
  (yaml-ts-mode . lsp-deffered)
  (yaml-ts-mode . display-line-numbers-mode))


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
  (corfu-auto-delay 0.5) ; Enable auto completion
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
   '(cape-dabbrev
     cape-keyword
     cape-dict))
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)))


;;;; GRAPHICS

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-week-agenda t)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package gruvbox-theme)

(use-package modus-themes
  :ensure t
  :custom
  ;; https://protesilaos.com/emacs/modus-themes
  (modus-vivendi-tritanopia-palette-overrides
   '((fringe unspecified)
     (bg-line-number-active unspecified)
     (bg-line-number-inactive unspecified)
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (bg-main "#171717")))
  :config
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
