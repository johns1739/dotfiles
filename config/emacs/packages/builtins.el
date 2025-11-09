;;; -*- lexical-binding: t -*-

;; Catch-all
(use-package emacs
  :straight nil
  :bind (:map global-leader-map
              (", U" . emacs-upgrade))
  :init
  (defvar emacs-upgrade-alist
    '(straight-pull-all
      straight-rebuild-all
      treesit-pull-all
      eglot-upgrade-eglot)
    "List of functions to run on upgrade action.")
  (defun emacs-upgrade ()
    "Run upgrade functions in `emacs-upgrade-alist`."
    (interactive)
    (dolist (fn emacs-upgrade-alist)
      (funcall fn))
    (message "Emacs upgrade complete.")))

(use-package diff-mode
  :straight nil
  :bind (:map diff-mode-map
              ("M-o" . nil)))

(use-package ediff
  :straight nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :bind (:map global-leader-map
              ("x d" . ediff-files)))

(use-package eglot
  :straight nil
  :bind (:map global-leader-map
              ("L" . eglot)
              ("l TAB" . eglot-format)
              ("l e" . eglot-events-buffer)
              ("l E" . eglot-stderr-buffer)
              ("l l" . eglot-reconnect)
              ("l q" . eglot-shutdown)
              ("l Q" . eglot-shutdown-all)
              ("l r" . eglot-rename)
              ("l d" . eglot-find-declaration)
              ("l a" . eglot-code-actions))
  :config
  (setq eglot-mode-line-session nil))

(use-package eshell
  :straight nil
  :bind (:map global-leader-map
              ("k e" . project-eshell)
              ("k E" . eshell))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eshell\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-min-height . 25))))

(use-package ffap
  :straight nil
  :commands (find-file-at-point)
  :init
  (defun ffap-deep-match-file (filename)
    (let ((project-dir (project-directory)))
      (or (and project-dir (ffap-deep-match-file-string filename project-dir))
          (ffap-deep-match-file-string filename default-directory))))
  (defun ffap-deep-match-file-string (filename dir)
    (let* ((deep-1 (f-join "**" filename))
           (deep-2 (f-join "**" "**" filename))
           (files  (or (file-expand-wildcards (expand-file-name deep-1 dir) t)
                       (file-expand-wildcards (expand-file-name deep-2 dir) t))))
      (and files (car files))))
  :config
  (require 'f)
  (add-to-list 'ffap-alist '("" . ffap-deep-match-file)))

(use-package flymake
  :straight nil
  :bind (:map global-leader-map
              ("D" . flymake-mode)
              ("d n" . flymake-goto-next-error)
              ("d p" . flymake-goto-prev-error)
              ("d d" . flymake-show-buffer-diagnostics)
              ("d D" . flymake-show-project-diagnostics))
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

(use-package flyspell
  :straight nil
  ;; brew install aspell
  ;; brew install ispell
  ;; ispell fails to install due to compilation issues
  :if (or (executable-find "aspell") (executable-find "ispell"))
  :bind (:map global-leader-map
              ("m 4" . flyspell-prog-mode)
              ("m $" . flyspell-mode))
  :config
  (if (executable-find "ispell")
      (setq ispell-program-name "ispell")
    (setq ispell-program-name "aspell")))

(use-package help
  :straight nil
  :bind (:map help-map
              ("h" . nil)) ;; accidentally pressed too often
  :custom
  (help-window-select 'other))

(use-package hippie-exp
  :straight nil
  :bind (:map global-map ("M-I" . hippie-expand))
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-list
     try-expand-line
     try-expand-dabbrev-visible
     try-expand-dabbrev
     ;; try-expand-list-all-buffers
     try-expand-line-all-buffers
     try-expand-dabbrev-all-buffers
     ;; try-expand-whole-kill ;; use M-y instead
     ;; try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name))
  :init
  (defadvice hippie-expand (around hippie-expand-case-fold)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))
  :config
  (ad-activate 'hippie-expand))

(use-package org
  ;; Useful documentation: https://orgmode.org/worg/org-syntax.html
  :straight nil
  :commands (org-todo-list
             org-agenda
             org-agenda-list
             org-capture
             org-capture-goto-target
             org-search-view
             org-occur-in-agenda-files)
  :init
  (defun org-mode-setup ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (emacs-lisp . t)
       (http . t)
       (shell . t)
       (scheme . t)
       (sql . t)
       (python . t)))
    (electric-indent-local-mode -1))
  :hook
  (org-mode . org-mode-setup)
  (org-agenda-mode . hl-line-mode)
  :bind (:map global-leader-map
              ("N" . org-agenda)
              ("n ," . org-capture-goto-last-stored)
              ("n /" . org-tags-view)
              ("n SPC" . org-search-view)
              ("n f" . org-capture-goto-target)
              ("n k" . org-capture)
              ("n L" . org-store-link)
              ("n s" . org-occur-in-agenda-files)
              ("n t" . org-todo-list)
              ("n v" . org-agenda-list)
              ("n r r" . org-refile-copy)
              ("n r R" . org-refile)
              ("n r ," . org-refile-goto-last-stored)
              :map org-mode-map
              ([remap goto-address-at-point] . org-open-at-point)
              ([remap kill-sentence] . org-cut-subtree)
              ("M-H" . org-babel-mark-block)
              ("M-n" . org-next-visible-heading)
              ("M-p" . org-previous-visible-heading)
              ("M-N" . org-babel-next-src-block)
              ("M-P" . org-babel-previous-src-block)
              ("M-P" . org-previous-visible-heading)
              ("C-M-N" . org-move-subtree-down)
              ("C-M-P" . org-move-subtree-up))
  :custom
  (org-directory "~/.notes")
  (org-agenda-files (list org-directory))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-on-weekday 0)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'far)
  (org-agenda-window-setup 'reorganize-frame)
  (org-columns-default-format "%TODO %ITEM %ALLTAGS %DEADLINE")
  (org-cycle-hide-block-startup t)
  (org-edit-src-content-indentation 0)
  (org-hide-drawer-startup t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars (display-graphic-p))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1)))
  (org-return-follows-link nil)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
  (org-todo-keyword-faces '(("TODO" . "steel blue")
                            ("ACTIVE" . "light goldenrod")
                            ("DONE" . "dim gray")
                            ("CANCELED" . "dim gray")))
  ;; https://orgmode.org/manual/Capture-templates.html
  (org-capture-templates
   `(("t" "Task" entry (file+headline "tasks.org" "Tasks") "* TODO %?\n"
      :prepend t :empty-lines-after 1)
     ("n" "Note" entry (file+headline "notes.org" "Notes") "* %?\n%i"
      :prepend t :empty-lines-after 1)
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %T %?\n%i"
      :prepend t :tree-type week)))
  :config
  (require 'org-capture)
  (require 'org-crypt))

(use-package proced
  :straight nil
  :commands proced
  :bind (:map global-leader-map ("k P" . proced))
  :custom
  (proced-auto-update-flag 'visible)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'short))

(use-package project
  :straight nil
  :bind (:map project-prefix-map
              ("K" . project-forget-project))
  :custom
  (project-switch-commands '((project-find-regexp "Regexp" "s")
                             (project-find-file "File" "f")
                             (project-find-dir "Dir" "d")
                             (project-kill-buffers "Kill" "k")))
  :init
  (keymap-set global-leader-map "p" project-prefix-map))

(use-package smerge-mode
  :straight nil
  :hook
  (prog-mode . smerge-mode))

(use-package tab-bar
  :straight nil
  :if (display-graphic-p)
  :bind (:map global-map
              ("s-{" . tab-previous)
              ("s-}" . tab-next)
              ("s-t" . tab-bar-new-tab)
              ("s-w" . tab-bar-close-tab)
              :map tab-prefix-map
              ("SPC" . tab-switch)
              ("'" . tab-recent)
              ("T" . tab-bar-mode))
  :init
  (keymap-set goto-map "t" tab-prefix-map)
  :custom
  (tab-bar-show 1)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-button-show nil)
  (tab-bar-close-button nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-button nil))

(use-package treesit
  :straight nil
  :init
  (defun treesit-pull-all ()
    "Install all language grammars registered with Treesitter"
    (interactive)
    (require 'treesit)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
  :commands (treesit-pull-all)
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (gleam "https://github.com/gleam-lang/tree-sitter-gleam/")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
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
          (scheme "https://github.com/6cdh/tree-sitter-scheme"))))

(use-package which-key
  :demand
  :straight nil
  :custom
  (which-key-side-window-location 'right)
  :config
  (which-key-mode))

(use-package xref
  :straight nil
  :defer
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily))
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package custom
  :straight nil
  :bind (:map global-leader-map
              ("x y" . copy-relative-file-name)
              ("x Y" . copy-absolute-file-name))
  :init
  (defun absolute-file-name ()
    "Absolute path to file."
    (expand-file-name (buffer-file-name)))
  (defun copy-absolute-file-name ()
    "Copy absolute file path of current buffer."
    (interactive)
    (let ((afn (absolute-file-name)))
      (kill-new (absolute-file-name))
      (message "Copied %s" afn)))
  (defun project-directory ()
    "Current project directory."
    (let ((project (project-current)))
      (and project (project-root project))))
  (defun relative-file-name ()
    "Relative from project or cwd directory."
    (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))
  (defun copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (let ((rfn (relative-file-name)))
      (kill-new (relative-file-name))
      (message "Copied %s" rfn))))
