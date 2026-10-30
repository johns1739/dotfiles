;;; emacs-packages.el --- Third Party Packages  -*- lexical-binding: t; -*-

(use-package ace-window
  ;; After calling ace-window, press ? for more actions
  :bind (([remap other-window] . ace-window))
  :custom
  (aw-scope (if (daemonp) 'frame 'visible))
  (aw-dispatch-when-more-than 2))

(use-package avy
  :bind (([remap goto-line] . avy-goto-line)
         :map global-leader-map
         ("y p" . avy-copy-line)
         ("y P" . avy-copy-region)
         ("y g" . avy-move-line) ;; g for grab
         ("y G" . avy-move-region) ;; G for Grab
         ("y k" . avy-kill-whole-line)
         ("y K" . avy-kill-region)
         ("y y" . avy-kill-ring-save-whole-line)
         ("y Y" . avy-kill-ring-save-region)
         :map isearch-mode-map
         ("M-g" . avy-isearch)
         :map goto-map
         ("G" . avy-resume)
         ("g" . avy-goto-char-2)))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package beframe
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :bind ( :map global-leader-map
          ("SPC" . beframe-switch-buffer))
  :config
  (beframe-mode 1))

(use-package cape ;; Cape provides Completion At Point Extensions
  :init
  ;; note: Excluded buggy #'cape-line
  (dolist (fn '(cape-elisp-block cape-file cape-dict cape-keyword cape-dabbrev))
    (add-hook 'completion-at-point-functions fn)))

(use-package claude-code-ide
  :if (executable-find "claude")
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ( :map global-leader-map
          ("i a" . claude-code-ide-menu)) ;; a for anthropic
  :init
  (with-eval-after-load 'project
    (project-add-switch-command #'claude-code-ide-dwim "Claude" "a"))
  (with-eval-after-load 'claude-code-ide-transient
    (transient-insert-suffix 'claude-code-ide-menu "s"
      '("RET" "Start or switch to Claude Code instance" claude-code-ide-dwim)))
  (defun claude-code-ide-dwim ()
    "Start or switch to a Claude Code IDE session."
    (interactive)
    (condition-case nil
        (claude-code-ide-switch-to-buffer)
      (user-error (claude-code-ide))))
  :custom
  (claude-code-ide-no-flicker t)
  (claude-code-ide-use-side-window nil)
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-window-side 'left)
  (claude-code-ide-vterm-render-delay 0.01) ; increase for smoother but less responsive
  (claude-code-ide-terminal-initialization-delay 0.2) ; better render
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package consult
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap imenu] . consult-imenu)
   ([remap Info-search] . consult-info)
   ([remap isearch-edit-string] . consult-isearch-history)
   ([remap recentf-open] . consult-recent-file)
   ([remap recentf] . consult-recent-file)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
   ([remap yank-pop] . consult-yank-pop))
  ( :map global-leader-map
    ("d SPC" . consult-flymake)
    ("n SPC" . consult-org-agenda)
    ;; Emacs settings
    (", SPC" . consult-emacs-packages)
    (", m" . consult-minor-mode-menu)
    (", s" . consult-emacs)
    (", t" . consult-theme)
    ;; Text Edits
    ("x \\" . align)
    ("x |" . align-current)
    ("x k" . consult-keep-lines)
    ("x f" . flush-lines)
    ("x s" . sort-lines)
    ("x u" . delete-duplicate-lines))
  ( :map minibuffer-mode-map
    ("C-r" . consult-history)
    ("C-M-i" . consult-history))
  ( :map project-prefix-map
    ("SPC" . consult-project-buffer))
  ( :map search-map
    (")" . consult-kmacro)
    ("f" . consult-find) ;; works even if not in a project
    ("F" . find-name-dired)
    ("I" . consult-imenu-multi)
    ("j". consult-register)
    ("l" . consult-line)
    ("L" . consult-line-multi)
    ("s" . consult-ripgrep)
    ("k" . consult-focus-lines)
    ("m" . consult-mark))
  ( :map goto-map
    ("SPC" . consult-buffer)
    ("o" . consult-outline)
    ("j" . consult-register-load)
    ("J" . consult-register-store)
    ("m" . consult-bookmark)
    ("M" . bookmark-set-no-overwrite)) ;; eg bookmarking multiple files with same name
  ( :map help-map
    ("SPC" . consult-info))
  :init
  (defun consult-emacs (&optional initial)
    "Search emacs configurations."
    (interactive)
    (consult-ripgrep (list (file-name-directory (file-truename user-init-file)) custom-file) initial))
  (defun consult-emacs-packages ()
    "Search emacs package configurations."
    (interactive)
    (consult-emacs "^(use-package\\ "))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (with-eval-after-load 'project
    (project-add-switch-command 'consult-project-buffer "Buffer" "SPC")
    (project-add-switch-command 'consult-ripgrep "Search" "s"))
  (if (executable-find "fd")
      (bind-keys :map search-map
                 ("f" . consult-fd))))

(use-package consult-ghostel
  :vc ( :url "https://github.com/dakra/ghostel"
        :lisp-dir "extensions/consult-ghostel"
        :rev :newest)
  :after (consult ghostel)
  :bind
  ( :map global-leader-map
    ("k SPC" . consult-ghostel)))

(use-package corfu
  :defer 2
  :bind ( :map corfu-map
          ("TAB" . corfu-complete)
          ("RET" . nil))
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-echo-delay 0.2)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(0.6 . 0.2))
  (corfu-preselect 'prompt)
  (corfu-preview-current 'prompt)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package dashboard
  :demand
  :if (daemonp) ;; Better when used w/ emacs server.
  :custom
  (initial-buffer-choice 'dashboard-open)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind ( :map global-map
          ([remap project-find-regexp] . deadgrep)
          :map search-map
          ("g" . deadgrep)
          :map deadgrep-mode-map
          ("C-w" . deadgrep-edit-mode)))

(use-package diff-hl ;; git diff changes in fringe
  :after magit
  :commands (diff-hl-show-hunk)
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-file-dispatch "d"
      '("H" "Diff Hunks" diff-hl-show-hunk))
    (transient-append-suffix 'magit-file-dispatch "d"
      '("h" "Diff Markers" diff-hl-mode)))
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders t)
  :config
  ;; meow's own shim advises `diff-hl-show-hunk-inline-popup', an obsolete
  ;; alias since diff-hl 0.11.0, so motion state is never entered. The
  ;; switch back to normal on `diff-hl-show-hunk-hide' still works.
  (with-eval-after-load 'meow
    (advice-add 'diff-hl-show-hunk-inline :before #'meow--switch-to-motion)))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'consult-xref)
  :init
  ;; (setopt dumb-jump-debug t)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; color-schemes
(use-package ef-themes :defer)
(use-package modus-themes :defer)
(use-package standard-themes :defer)
(use-package doric-themes :defer)

(use-package elfeed
  :commands (elfeed)
  :bind ( :map global-leader-map
          ("o f" . elfeed))
  :custom
  (elfeed-db-directory (expand-cache-path "elfeed"))
  (elfeed-feeds
   '(("http://nullprogram.com/feed/" null emacs)
     ("https://planet.emacslife.com/atom.xml" emacslife emacs)
     ("https://modern-sql.com/feed" modernsql sql)
     ;; ("https://www.reddit.com/r/ExperiencedDevs/top/.rss?t=month" reddit news)
     ;; ("https://hnrss.org/frontpage" hn news)
     ;; ("https://hnrss.org/jobs" hn jobs)
     ("https://lobste.rs/rss" lobste news))))

(use-package envrc
  ;; Must activate at the end
  :hook (after-init . envrc-global-mode))

(use-package find-file-in-project ;; project-aware ffip
  ;; https://github.com/redguardtoo/find-file-in-project
  :bind
  ( :map goto-map
    ("u" . find-file-in-project-at-point)
    ("U" . find-file-in-project-by-selected))
  :config
  (if (executable-find "fd")
      (setopt ffip-use-rust-fd t)))

(use-package forge
  ;; https://docs.magit.vc/forge/
  ;; setup:
  ;; Create ~/.authinfo with content:
  ;; machine api.github.com login USERNAME^forge password TOKEN
  ;; where USERNAME: git config --global github.user jubajr17
  ;; and TOKEN: from https://github.com/settings/tokens
  ;;            in a browser to generate a new "classic" token using
  ;;            the repo, user and read:org scopes
  ;; Run M-x auth-source-forget-all-cached (auth-source-forget-all-cached)
  :commands (forge-dispatch)
  :custom
  (forge-database-file (expand-cache-path "forge/forge-database.sqlite")))

(use-package format-all
  ;; https://github.com/lassik/emacs-format-all-the-code#supported-languages
  :commands (format-all-mode format-all-region-or-buffer)
  :bind ( :map global-leader-map
          ("TAB" . format-all-region-or-buffer)
          ("m TAB" . format-all-mode))
  :custom
  (format-all-show-errors 'errors))

(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel" :lisp-dir "lisp" :rev :newest)
  :bind ( :map global-leader-map
          ("k t" . ghostel-project)
          ("k T" . ghostel)
          :map project-prefix-map
          ("t" . ghostel-project)
          :map ghostel-semi-char-mode-map
          ("M-o" . ace-window)
          ("M-'" . meow-last-buffer)
          ("M-q" . meow-quit))
  :hook
  (after-init . ghostel-compile-global-mode)
  (after-init . ghostel-comint-global-mode)
  (ghostel-mode . ghostel-mode-setup)
  :init
  (defun ghostel-mode-setup ()
    (meow-mode -1))
  (defun ghostel-close-buffer (_buffer _event)
    (meow-quit))
  :config
  (add-hook 'ghostel-exit-functions #'ghostel-close-buffer)
  (with-eval-after-load 'project
    (project-add-switch-command #'ghostel-project "Ghostel" "t")))

(use-package git-link
  :commands (git-link git-link-dispatch)
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-file-dispatch
      "e" '("y" "Copy Link" git-link))
    (transient-append-suffix 'magit-file-dispatch
      "y" '("Y" "Copy Link Dispatch" git-link-dispatch))))

(use-package git-timemachine
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :hook (git-timemachine-mode . git-timemachine-toggle-meow-state)
  :init
  (transient-append-suffix 'magit-file-dispatch
    "d" '("T" "Timemachine" git-timemachine))
  (defun git-timemachine-refontify (&rest _)
    "Re-fontify buffer after timemachine revision change."
    (font-lock-ensure))
  (defun git-timemachine-toggle-meow-state ()
    "Set meow to motion state when enterint timemachine."
    (if git-timemachine-mode
        (progn
          (meow-motion-mode 1)
          (git-timemachine-refontify))
      (meow-normal-mode 1)))
  :config
  (advice-add 'git-timemachine-show-revision :after #'git-timemachine-refontify))

(use-package golden-ratio ;; auto-scales focused buffer
  :bind
  ( :map global-leader-map
    ("m G" . golden-ratio-mode))
  :custom
  (golden-ratio-auto-scale nil) ;; yields wider buffers, better
  :config
  (with-eval-after-load 'gptel-aibo
    (add-to-list 'golden-ratio-extra-commands 'gptel-aibo))
  (with-eval-after-load 'ace-window
    (add-to-list 'golden-ratio-extra-commands 'ace-window)))

(use-package gptel ;; ai llm copilot chatgpt
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((markdown-ts-mode . "### ") (org-mode . "* PROMPT ")))
  (gptel-response-prefix-alist '((org-mode . "** RESPONSE\n")))
  (gptel-gh-token-file (expand-cache-path "gptel/copilot-chat/token"))
  (gptel-gh-github-token-file (expand-cache-path "gptel/copilot-chat/github-token"))
  (gptel-crowdsourced-prompts-file (expand-cache-path "gptel/crowdsourced-prompts.csv"))
  :bind
  ( :map global-leader-map
    ("i g SPC" . gptel)
    ("i g :" . gptel-menu)
    ("i g A" . gptel-add)
    ("i g K" . gptel-context-remove-all)
    ("i g R" . gptel-rewrite))
  ( :map gptel-mode-map
    ("C-c C-<return>" . gptel-send)
    ("C-c C-c" . gptel-abort)
    ("C-c C-t" . gptel-org-set-topic)
    ("C-c C-p" . gptel-org-set-properties))
  :hook
  (gptel-mode . visual-line-mode)
  (gptel-mode . gptel-highlight-mode))

(use-package gptel-aibo
  :bind
  ( :map global-leader-map
    ("i g g" . gptel-aibo)
    ("i g I" . gptel-aibo-complete-at-point))
  ( :map gptel-aibo-mode-map
    ("C-c C-<return>" . gptel-aibo-send))
  :config
  (require 'gptel))

(use-package gptel-magit ;; auto-generate commit messages
  :after magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (require 'gptel))

(use-package gptel-quick ;; quick summary of things
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
  :custom
  (gptel-quick-display 'echo)
  (gptel-quick-use-context t)
  (gptel-quick-timeout 12)
  :bind ( :map global-leader-map
          ("i g ." . gptel-quick))
  :config
  (require 'gptel))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("k" . helpful-key) ;; overshadowed by meow
         ("." . helpful-at-point)
         ("F" . helpful-function))
  :config
  (add-to-list 'display-buffer-alist
               '((major-mode . helpful-mode)
                 (display-buffer-reuse-mode-window))))

(use-package indent-bars
  :bind
  ( :map global-leader-map
    ("m g" . indent-bars-mode)))

(use-package inheritenv ;; Make Emacs temp buffers inherit buffer-local environment variables
  :defer
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package jinx
  :if (executable-find "enchant-2")
  :bind ( ("C-M-$" . jinx-languages)
          ([remap flyspell-mode] . jinx-mode)
          ([remap ispell-word] . jinx-correct))
  :custom
  (jinx-delay 0.5))

(use-package keychain-environment
  :if (eq system-type 'darwin) ;; macos
  :config
  (keychain-refresh-environment))

(use-package magit
  :commands (magit-project-status)
  :bind ( :map global-leader-map
          ("j" . magit-file-dispatch)
          ("J" . magit-dispatch)
          :map magit-status-mode-map
          ("C-o" . magit-diff-visit-file-other-window))
  :init
  (with-eval-after-load 'project
    (project-add-switch-command 'magit-project-status "Magit" "j"))
  :custom
  (magit-blame-echo-style 'headings)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun magit-diff-main-branch ()
    (interactive)
    (pcase-let ((`(,arguments ,files) (magit-diff-arguments))
                (file (magit-file-relative-name)))
      (magit-diff-range (concat "origin/" (magit-main-branch) "...HEAD")
                        arguments
                        (or files (and file (list file))))))
  (transient-append-suffix 'magit-diff "r"
    '("m" "Diff origin/main...HEAD" magit-diff-main-branch)))

(use-package marginalia
  :demand
  :custom
  (completions-detailed nil)
  :config
  (marginalia-mode))

(use-package meow
  :demand
  :bind
  ( :map global-map
    ("M-q" . meow-quit)
    ("M-'" . meow-last-buffer))
  :custom
  (meow-use-clipboard t)
  (meow-select-on-append t)
  (meow-keypad-self-insert-undefined nil)
  (meow-expand-hint-remove-delay 2)
  (meow-cursor-type-motion '(hbar . 2))
  (meow-goto-line-function #'avy-goto-line)
  :init
  (defun meow-search-reverse ()
    (interactive)
    (unless (meow--direction-backward-p)
      (meow-reverse))
    (call-interactively #'meow-search))
  (defun meow-setup ()
    (set-face-attribute 'meow-insert-indicator nil :inherit '(bold-italic warning))
    (set-face-attribute 'meow-beacon-indicator nil :inherit '(bold success))
    (set-face-attribute 'meow-motion-indicator nil :inherit 'italic)
    (dolist (mode '(help-mode csv-mode vterm-mode ghostel-mode))
      (add-to-list 'meow-expand-exclude-mode-list mode))
    (meow-motion-overwrite-define-key ;; Deprecated: use meow-motion-define-key on new version 1.6
     (cons "SPC" global-leader-map)
     '("M-SPC" . "H-SPC") ;; Rebind original space command (e.g., magit-status)
     '("<escape>" . ignore))
    (setq meow-paren-keymap (make-keymap))
    (meow-define-state paren
      "meow state for structural sexp editing"
      :lighter " [P]"
      :keymap meow-paren-keymap)
    (setq meow-cursor-type-paren 'hollow)
    (meow-define-keys 'paren
      '("<escape>" . meow-normal-mode)
      '("l" . forward-sexp)
      '("h" . backward-sexp)
      '("j" . down-list)
      '("k" . backward-up-list)
      '("r" . raise-sexp)
      '("s" . delete-pair)
      '("t" . transpose-sexps)
      '("w" . insert-parentheses)
      '("u" . meow-undo)
      '(")" . meow-forward-slurp)
      '("(" . meow-backward-slurp)
      '(">" . meow-forward-barf)
      '("<" . meow-backward-barf)
      '("[" . meow-wrap-square)
      '("{" . insert-pair)
      '("\"" . meow-wrap-string)
      '("S" . meow-split-sexp)
      '("J" . meow-join-sexp))
    (meow-normal-define-key
     (cons "SPC" global-leader-map)
     '("M-DEL" . meow-backward-kill-word)
     '("TAB" . meow-indent)
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("%" . meow-query-replace)
     '("M-%" . meow-query-replace-regexp)
     '("-" . negative-argument)
     '("_" . meow-reverse)
     '("(" . meow-start-kmacro)
     '(")" . meow-end-or-call-kmacro)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-change-save)
     '("d" . meow-delete)
     '("D" . meow-kill)
     '("M-d" . meow-kill-word)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-till)
     '("F" . meow-find)
     (cons "g" goto-map)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-back-to-indentation)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-join)
     '("k" . meow-prev)
     '("K" . meow-kmacro-lines)
     '("M-k" . meow-kill-symbol)
     '("M-K" . meow-kill-append)
     '("l" . meow-right)
     '("L" . move-end-of-line)
     '("m" . meow-pop-to-mark)
     '("M" . meow-unpop-to-mark)
     '("n" . meow-search)
     '("N" . meow-search-reverse)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . nil) ;; Keep q unbound for other apps to bind.
     '("Q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-replace-save)
     '("M-r" . meow-replace-pop)
     (cons "s" search-map)
     '("S" . save-buffer)
     '("t" . nil)
     '("T" . meow-swap-grab)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-page-down)
     '("V" . meow-page-up)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-kill-whole-line)
     '("y" . meow-save)
     '("Y" . meow-save-append)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("'" . meow-last-buffer)
     '(";" . meow-comment)
     '(":" . meow-goto-line)
     '("[" . nil)
     '("]" . nil)
     '("/" . meow-visit)
     '("," . meow-inner-of-thing)
     '("<" . meow-beginning-of-thing)
     '("." . meow-bounds-of-thing)
     '(">" . meow-end-of-thing)
     '("<escape>" . meow-cancel-selection)
     '("<backspace>" . meow-backspace)))
  :config
  (meow-setup)
  (meow-setup-indicator)
  (meow-global-mode))

(use-package meow-tree-sitter
  :after meow
  :commands (meow-tree-sitter-node)
  :init
  (meow-normal-define-key
   '("o" . meow-tree-sitter-node))
  :config
  (meow-tree-sitter-register-defaults))

(use-package mpv ;; media player https://mpv.io/
  :vc (:url "https://github.com/kljohann/mpv.el" :branch "master")
  :defer
  :custom
  (mpv-default-options '("--audio-channels=stereo")))

(use-package orderless
  :custom
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  (completion-styles '(orderless basic)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package pinentry ;; allows for secure entry of passphrases requested by GnuPG
  :after magit
  :config
  (pinentry-start))

(use-package spacious-padding
  :bind ( :map global-leader-map
          ("m P" . spacious-padding-mode)))

(use-package tmr
  :bind ( :map global-leader-map
          ("o t" . tmr-tabulated-view))
  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer
     tmr-acknowledge-minibuffer))
  :config
  (if (executable-find "ffplay")
      (add-to-list 'tmr-timer-finished-functions 'tmr-sound-play))
  (if (featurep 'dbusbind)
      (add-to-list 'tmr-timer-finished-functions 'tmr-notification-notify))
  (tmr-mode-line-mode t))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package visual-fill-column ;; wrap at fill-column boundary.
  :defer
  ;; (visual-fill-column-center-text t) ;; Conflicts & hides diff-hl's margins
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline))

(use-package visual-replace
  :bind (([remap query-replace] . visual-replace)
         ([remap replace-string] . visual-replace)
         ([remap isearch-query-replace] . visual-replace-from-isearch)
         ([remap isearch-query-replace-regexp] . visual-replace-from-isearch)
         :map search-map
         ("%" . visual-replace-selected)))

(use-package vundo
  :bind ( ("C-x u" . vundo)))

(use-package web-server ;; GNU ELPA build lacks `ws-process' needed by claude-code-ide
  :vc (:url "https://github.com/eschulte/emacs-web-server" :branch "master")
  :defer)

(use-package whisper ;; Speech-To-Text; STT
  :vc (:url "https://github.com/natrys/whisper.el" :branch "master")
  :bind ( :map global-map
          ("<f9>" . whisper-run))
  :custom
  (whisper-install-directory (expand-cache-path "whisper/install/"))
  (whisper-model "medium.en")
  (whisper-language "en")
  (whisper-return-cursor 'start)
  (whisper-return-cursor-to-start nil) ;; allows me to keep typing
  (whisper-use-threads (1- (num-processors)))
  :hook
  (whisper-before-transcription . whisper-set-recording-filename)
  :init
  (defvar whisper-recordings-dir (expand-cache-path "whisper/recordings/")
    "Whisper directory to store recordings.")
  (defun whisper-set-recording-filename ()
    (setq whisper--temp-file
          (expand-file-name (format-time-string "%Y-%m-%d %H:%M:%S.wav")
                            whisper-recordings-dir)))
  (defun whisper-replay (&optional file)
    "Replay the last temporary recoring."
    (interactive (list
                  (when current-prefix-arg
                    (read-file-name "File: " whisper-recordings-dir))))
    (setq whisper--temp-file (or file whisper--temp-file))
    (mpv-play whisper--temp-file))
  (defun whisper-sox-record-command (record-command output-file)
    "Record with sox; FFmpeg's avfoundation input drops audio buffers."
    (if whisper--ffmpeg-input-file
        (funcall record-command output-file)
      `("rec" "-q" "-V1" "-c" "1" "-r" "16000" "-b" "16" ,output-file
        "trim" "0" ,(number-to-string whisper-recording-timeout))))
  (when (executable-find "rec")
    (advice-add 'whisper--record-command :around #'whisper-sox-record-command))
  (with-eval-after-load 'ghostel
    (bind-keys :map ghostel-semi-char-mode-map
               ("<f9>" . whisper-run)))
  :config
  (make-directory (expand-cache-path whisper-recordings-dir) t))

(use-package writeroom-mode
  :bind ( :map global-leader-map
          ("m w" . writeroom-mode)
          ("m W" . global-writeroom-mode))
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width fill-column)
  ;; Default reserves room for the line numbers we are about to hide.
  (writeroom-added-width-left 0)
  (writeroom-local-effects '(writeroom-toggle-line-numbers))
  (writeroom-mode-line t)
  :init
  (defvar-local writeroom-line-numbers-restore nil)
  (defun writeroom-toggle-line-numbers (arg)
    "Hide line numbers while `writeroom-mode' is active."
    (cond ((> arg 0)
           (setq writeroom-line-numbers-restore (bound-and-true-p display-line-numbers-mode))
           (display-line-numbers-mode -1))
          (writeroom-line-numbers-restore
           (display-line-numbers-mode 1)))))

(use-package xclip
  :demand
  :config
  (xclip-mode))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :defer 2
  :bind ( :map goto-map
          ("&" . yas-visit-snippet-file)
          :map global-leader-map
          ("x &" . yas-new-snippet))
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package zoxide
  :bind
  ( :map search-map
    ("d" . zoxide-travel)
    ("D" . zoxide-find-file)))

(provide 'emacs-packages)
;;; emacs-packages.el ends here
