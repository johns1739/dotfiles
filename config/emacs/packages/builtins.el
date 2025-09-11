;;; -*- lexical-binding: t -*-

(use-package eglot
  :straight nil
  :bind (:map global-leader-map
              ("L" . eglot)
              ("l l" . eglot-reconnect)
              ("l q" . eglot-shutdown)
              ("l Q" . eglot-shutdown-all)
              ("l r" . eglot-rename)
              ("l d" . eglot-find-declaration)
              ("l a" . eglot-code-actions))
  :init
  (defun eglot-setup ()
    (bind-keys :map (current-local-map)
               ([remap indent-format-buffer] . eglot-format-buffer)))
  :hook
  (eglot-managed-mode . eglot-setup))

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
              ("k d" . flymake-show-buffer-diagnostics)
              ("k D" . flymake-show-project-diagnostics))
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

(use-package flyspell
  :straight nil
  ;; brew install aspell
  ;; brew install ispell
  ;; ispell fails to install due to compilation issues
  :if (or (executable-find "aspell") (executable-find "ispell"))
  :bind (:map global-leader-map
              ("; $" . flyspell-mode))
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
    (bind-keys :map (current-local-map)
               ([remap goto-address-at-point] . org-open-at-point)
               ([remap kill-sentence] . org-cut-subtree))
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
              ("n ," . org-capture-goto-last-stored)
              ("n /" . org-tags-view)
              ("n SPC" . org-search-view)
              ("n f" . org-capture-goto-target)
              ("n k" . org-capture)
              ("n L" . org-store-link)
              ("n n" . org-agenda)
              ("n s" . org-occur-in-agenda-files)
              ("n t" . org-todo-list)
              ("n v" . org-agenda-list)
              ("n W" . org-refile)
              :map org-mode-map
              ("C-c M-h" . org-babel-mark-block)
              ("M-n" . org-next-visible-heading)
              ("M-p" . org-previous-visible-heading)
              ("M-N" . org-move-subtree-down)
              ("M-P" . org-move-subtree-up))
  :custom
  (org-directory "~/.notes")
  (org-agenda-files (list org-directory))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'far)
  (org-agenda-window-setup 'reorganize-frame)
  (org-columns-default-format "%TODO %ITEM %ALLTAGS %DEADLINE")
  (org-cycle-hide-block-startup t)
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
  (org-todo-keywords
   '((sequence "TODO(t!)" "ACTIVE(a!)" "|" "DONE(d!)" "CANCELED(c@)")))
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
  (require 'org-capture))

(use-package proced
  :straight nil
  :commands proced
  :bind (("C-M-p" . proced))
  :custom
  (proced-auto-update-flag 'visible)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'short))

(use-package project
  :straight nil
  :bind (:map project-prefix-map
              ("y" . copy-relative-file-name)
              ("Y" . copy-absolute-file-name)
              ("K" . project-forget-project)
              :map global-leader-map
              ("s f" . project-find-file)
              ("s s" . project-find-regexp)
              ("s d" . project-find-dir))

  :custom
  (project-switch-commands '((project-find-regexp "Regexp" "g")
                             (project-find-file "File" "f")
                             (project-find-dir "Directory" "d")
                             (project-kill-buffers "Kill" "k")))
  :init
  (keymap-set global-leader-map "p" project-prefix-map)
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
  (defun treesit-pull-languages ()
    "Install all language grammars registered with Treesitter"
    (interactive)
    (require 'treesit)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
  :commands (treesit-pull-languages)
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
  :hook (after-init . which-key-mode)
  :straight nil)

(use-package xref
  :straight nil
  :defer
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily))
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))
