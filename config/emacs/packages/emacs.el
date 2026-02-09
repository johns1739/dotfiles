;;; -*- lexical-binding: t -*-

(use-package emacs ;; graphics only
  :demand
  :straight nil
  :if (display-graphic-p)
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode)
  :init
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 120))
  :custom
  (prettify-symbols-alist '(("!=" . ?≠)
                            ;; ("&&" . ?∧)
                            ("->" . ?→)
                            ("->>" . ?↠)
                            ("<-" . ?←)
                            ("<<" . ?«)
                            ("<=" . ?≤)
                            ("<>" . ?◇)
                            ("<|" . ?◁)
                            ;; ("==" . ?≡)
                            ("=>" . ?⇒)
                            (">=" . ?≥)
                            (">>" . ?»)
                            ;; ("not" . ?¬)
                            ("|>" . ?▷)
                            ;; ("||" . ?∨)
                            )))

(use-package emacs ;; terminal only
  :demand
  :straight nil
  :unless (display-graphic-p)
  :config
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(use-package emacs
  :demand
  :straight nil
  :init
  (defvar-keymap global-leader-map :doc "Global leader keymap.")
  (keymap-set ctl-x-map "SPC" global-leader-map)
  (keymap-set global-leader-map "g" goto-map)
  (keymap-set global-leader-map "s" search-map)
  :bind ( :map global-map
          ([remap backward-sentence] . backward-sexp)
          ([remap forward-sentence] . forward-sexp)
          ([remap split-window-below] . split-window-below-and-jump)
          ([remap split-window-right] . split-window-right-and-jump)
          ([remap downcase-word] . downcase-dwim)
          ([remap upcase-word] . upcase-dwim)
          ("C-M-;" . comment-indent)
          ("C-j" . comment-indent-new-line)
          ("M-I" . completion-at-point)
          ("M-j" . join-line)
          ("M-L" . duplicate-dwim)
          ("M-n" . forward-paragraph)
          ("M-o" . other-window)
          ("M-p" . backward-paragraph)
          :map global-leader-map
          ("TAB" . indent-format-buffer)
          ("SPC" . project-switch-to-buffer)
          ("=" . balance-windows-area)
          ("0" . delete-window)
          ("1" . delete-other-windows)
          ("2" . split-window-below-and-jump)
          ("3" . split-window-right-and-jump)
          ;; Copy/Paste/Edits
          ("x l" . keep-lines)
          ("x k" . delete-matching-lines)
          ("x u" . delete-duplicate-lines)
          ("x s" . sort-lines)
          ("x w" . whitespace-cleanup)
          ;; Compilation & Computation
          ("k *" . calc)
          ("k r" . ielm)
          ;; Settings (Look & Feel)
          (", ," . open-custom-file)
          (", <" . open-emacs-file)
          (", +" . global-text-scale-adjust)
          (", =" . balance-windows-area)
          (", D" . toggle-debug-on-error)
          (", F" . toggle-frame-fullscreen)
          (", R" . restart-emacs)
          (", SPC" . load-theme)
          (", c" . display-fill-column-indicator-mode)
          (", f" . toggle-frame-maximized)
          (", h" . hl-line-mode)
          (", n" . display-line-numbers-mode)
          (", r" . reload-emacs)
          (", t" . toggle-truncate-lines)
          (", x" . describe-font)
          :map mode-specific-map
          ("C-o" . goto-address-at-point)
          :map goto-map
          ("SPC" . switch-to-buffer)
          ("." . xref-find-definitions)
          (">" . eldoc)
          ("," . xref-go-back)
          (";" . scratch-buffer)
          (":" . goto-line)
          ("?" . xref-find-references)
          ("/" . xref-find-apropos)
          ("'" . mode-line-other-buffer)
          ("f" . find-file-at-point)
          ("d" . dired-jump)
          ("i" . imenu)
          ("j" . jump-to-register)
          ("J" . point-to-register)
          ("l" . goto-line)
          ("m" . bookmark-jump)
          ("M" . bookmark-set)
          ;; Window navigation
          ("w h" . windmove-left)
          ("w j" . windmove-down)
          ("w k" . windmove-up)
          ("w l" . windmove-right)
          ("w H" . windmove-swap-states-left)
          ("w J" . windmove-swap-states-down)
          ("w K" . windmove-swap-states-up)
          ("w L" . windmove-swap-states-right)
          :map search-map
          ("b" . ibuffer)
          ("g" . rgrep)
          ("j" . list-registers)
          ("m" . list-bookmarks)
          ("o" . occur)
          ("r" . recentf-open))
  :custom
  (auto-revert-avoid-polling t)
  (auto-window-vscroll nil)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (basic partial-completion)))))
  (completion-cycle-threshold 3)
  (completion-ignore-case t)
  (completion-styles '(basic substring partial-completion))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height 20)
  (confirm-kill-emacs 'y-or-n-p)
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (display-time-default-load-average nil)
  (duplicate-line-final-position 1)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (enable-recursive-minibuffers t) ;; Might be confusing
  (fast-but-imprecise-scrolling t)
  (global-auto-revert-non-file-buffers t)
  (history-delete-duplicates t)
  (history-length 1000)
  (imenu-max-item-length 80)
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (initial-scratch-message nil)
  (isearch-wrap-pause 'no)
  (kill-do-not-save-duplicates t)
  (max-mini-window-height 0.2)
  (next-error-find-buffer-function 'next-error-buffer-unnavigated-current)
  (next-error-highlight 1.0)
  (next-error-highlight-no-select 1.0)
  (next-error-message-highlight t)
  (next-error-recenter '(4))
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 100)
  (register-preview-delay 0.5)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (scroll-conservatively most-positive-fixnum)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (show-paren-context-when-offscreen 'show-paren-context-when-offscreen)
  (tab-always-indent t)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-handled-backends '(Git))
  :hook
  (compilation-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (special-mode . hl-line-mode)
  (before-save . nuke-trailing-whitespace)
  :config
  (setq-default cursor-type 'bar)
  (setq-default display-fill-column-indicator-column 100)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil) ;; use spaces instead of tabs
  (setq-default tab-width 4)
  (auto-save-visited-mode -1) ;; auto-format constantly triggers, annoying
  (column-number-mode -1)
  (delete-selection-mode -1)
  (desktop-save-mode -1) ;; CPU heavy when loading many buffers under LSP
  (electric-indent-mode t)
  (electric-pair-mode -1)
  (global-auto-revert-mode t)
  (global-eldoc-mode t)
  (global-so-long-mode t)
  (line-number-mode t)
  (pixel-scroll-precision-mode t)
  (recentf-mode 1)
  (repeat-mode -1) ;; Sometimes gets in the way.
  (save-place-mode t)
  (savehist-mode t)
  (window-divider-mode (display-graphic-p))
  (defun open-init-file ()
    (interactive)
    (find-file user-init-file))
  (defun open-emacs-file ()
    (interactive)
    (find-file (locate-user-emacs-file "packages/emacs.el")))
  (defun open-custom-file ()
    (interactive)
    (if (boundp 'custom-file)
        (find-file custom-file)))
  (defun split-window-below-and-jump ()
    "Split window below and jump to it."
    (interactive)
    (select-window (split-window-below)))
  (defun split-window-right-and-jump ()
    "Split window right and jump to it."
    (interactive)
    (select-window (split-window-right)))
  (defun indent-format-buffer ()
    (interactive)
    (save-excursion
      (whitespace-cleanup)
      (indent-region (point-min) (point-max) nil)))
  (defun reload-emacs ()
    (interactive)
    (load (locate-user-emacs-file "init.el") :no-error-if-file-is-missing))
  (defun nuke-trailing-whitespace ()
    ;; Running delete-trailing-whitespace on certain special modes can cause issues.
    ;; So only run in prog-mode.
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace))))

(use-package compile
  :bind (:map global-leader-map
              ("k ." . compile)
              ("k >" . comint)
              ("k ," . compilation-goto-in-progress-buffer)
              ("k b" . eval-buffer)
              ("k g" . recompile)
              ("k k" . compile-dwim)
              ("k K" . comint-dwim)
              ("k n" . next-error)
              ("k p" . previous-error)
              ("k w" . send-region-to-process))
  :custom
  (compilation-always-kill t)
  (compilation-context-lines 10)
  (compilation-error-regexp-alist '())
  (compilation-error-regexp-alist-alist '())
  (compilation-max-output-line-length 121)
  (compilation-scroll-output t)
  (compilation-search-path '(nil)) ;; directories to search for files
  (compilation-window-height 20)
  (compile-command "make ")
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :init
  (defun comint ()
    (interactive)
    (universal-argument)
    (command-execute #'compile))
  (defun compile-dwim ()
    (interactive)
    (if (project-current)
        (call-interactively #'project-compile)
      (call-interactively #'compile)))
  (defun comint-dwim ()
    (interactive)
    (universal-argument)
    (command-execute #'compile-dwim))
  (defun send-region-to-process (arg beg end)
    """
    Send the current region to a process buffer.
    The first time it's called, will prompt for the buffer to
    send to. Subsequent calls send to the same buffer, unless a
    prefix argument is used (C-u), or the buffer no longer has an
    active process.
    """
    (interactive "P\nr")
    (if (or arg ;; user asks for selection
            (not (boundp 'send-region-to-process-target)) ;; target not set
            ;; or target is not set to an active process:
            (not (process-live-p (get-buffer-process
                                  send-region-to-process-target))))
        (setq send-region-to-process-target
              (completing-read
               "Process: "
               (seq-map (lambda (el) (buffer-name (process-buffer el)))
                        (process-list)))))
    (process-send-region send-region-to-process-target beg end))
  :config
  ;; Make dir local variables work in compilation buffers
  (add-hook 'compilation-mode-hook 'hack-dir-local-variables-non-file-buffer)
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  ;; (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target
                 "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]" 1 2 nil nil 1))
  ;; (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target
                 "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))

(use-package diff-mode
  :straight nil
  :commands (diff diff-mode)
  :bind ( :map diff-mode-map
          ("M-o" . nil)))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :hook
  (dired-mode . hl-line-mode))

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
  (add-to-list 'display-buffer-alist '("\\*.*eshell\\*" (display-buffer-in-side-window))))

(use-package ffap
  :straight nil
  :commands (find-file-at-point)
  :init
  (defun ffap-deep-match-file (filename)
    (let ((project-dir (project-directory)))
      (or (and project-dir (ffap-deep-match-file-string filename project-dir))
          (ffap-deep-match-file-string filename default-directory))))
  (defun ffap-deep-match-file-string (filename dir)
    (let* ((deep-1 (file-name-concat "**" filename))
           (deep-2 (file-name-concat "**" "**" filename))
           (files  (or (file-expand-wildcards (expand-file-name deep-1 dir) t)
                       (file-expand-wildcards (expand-file-name deep-2 dir) t))))
      (and files (car files))))
  :config
  (add-to-list 'ffap-alist '("" . ffap-deep-match-file)))

(use-package files ;; backups
  :straight nil
  :custom
  (backup-by-copying t)
  (backup-directory-alist `(("." . "~/.backups")))
  (create-lockfiles nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 3)
  (make-backup-files t)
  (vc-make-backup-files t)
  (version-control t)
  :config
  (defun buffer-backed-up-set-to-time ()
    "Set the buffer-backed-up variable to the current time if t."
    (if (eq buffer-backed-up t)
        (setq buffer-backed-up (current-time))))
  (defun buffer-backed-up-reset-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (buffer-backed-up-set-to-time)
    (if (and buffer-backed-up
             (time-less-p (time-add (buffer-backed-up) (* 60 60 24))
                          (current-time)))
        (setq buffer-backed-up nil))
    (let ((orig-fun-result (apply orig-fun args)))
      (buffer-backed-up-set-to-time)
      orig-fun-result))
  (advice-add 'backup-buffer :around #'buffer-backed-up-reset-advice))

(use-package flymake
  :straight nil
  :bind (:map global-leader-map
              ("k d" . flymake-show-buffer-diagnostics)
              ("k D" . flymake-show-project-diagnostics))
  :custom
  (flymake-fringe-indicator-position 'left-fringe))

(use-package flyspell
  :straight nil
  ;; brew install aspell
  ;; brew install ispell
  ;; ispell fails to install due to compilation issues
  :if (or (executable-find "aspell") (executable-find "ispell"))
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
  :bind (:map global-map ("M-i" . hippie-expand))
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-list
     try-expand-line
     try-expand-dabbrev
     ;; try-expand-list-all-buffers
     try-expand-line-all-buffers
     try-expand-dabbrev-all-buffers
     ;; try-expand-whole-kill ;; use M-y instead
     ;; try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name))
  :init
  (defun hippie-expand-case-fold-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-fold-advice))

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
     '((emacs-lisp . t)
       (shell . t)
       (sql . t)))
    (electric-indent-local-mode -1))
  :hook
  (org-mode . org-mode-setup)
  (org-agenda-mode . hl-line-mode)
  :bind ( ("C-c L" . org-store-link)
          :map global-leader-map
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
          ;; ([remap kill-sentence] . org-cut-subtree)
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
  (org-agenda-todo-list-sublevels t)
  (org-agenda-window-setup 'reorganize-frame)
  (org-columns-default-format "%TODO %ITEM %ALLTAGS %DEADLINE")
  (org-cycle-hide-block-startup t)
  (org-edit-src-content-indentation 0)
  (org-hide-drawer-startup t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars (display-graphic-p))
  (org-log-done nil)
  (org-log-into-drawer t)
  (org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1)))
  (org-return-follows-link nil)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-agenda-sorting-strategy '((todo urgency-down category-keep deadline-up)))
  ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
  (org-todo-keyword-faces '(("TODO" . "steel blue")
                            ("ACTIVE" . "light goldenrod")
                            ("REVIEW" . "goldenrod")
                            ("BLOCKED" . "goldenrod")
                            ("DONE" . "dim gray")
                            ("BACKLOG" . "dim gray")
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
  (unless (file-exists-p "~/.notes")
    (make-directory "~/.notes"))
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
  :bind ( :map goto-map
          ("D" . project-dired)
          :map search-map
          ("d" . project-find-dir)
          :map project-prefix-map
          ("K" . project-forget-project))
  :custom
  (project-switch-commands '((project-find-regexp "Regexp" "g")
                             (project-find-file "File" "f")
                             (project-find-dir "Dir" "d")
                             (project-eshell "Eshell" "e")
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
