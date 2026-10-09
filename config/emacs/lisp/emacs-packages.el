;;; emacs-packages.el --- Third Party Packages  -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :demand
  :if (or (daemonp)
          (and (display-graphic-p) (memq window-system '(mac ns x pgtk))))
  :custom
  ;; (exec-path-from-shell-debug t)
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package ace-window
  :bind  (([remap other-window] . ace-window)
          :map goto-map
          ("w 0" . ace-delete-window)
          ("w 1" . ace-delete-other-windows)
          ("w o" . ace-select-window)
          ("w O" . ace-swap-window))
  :custom
  (aw-scope (if (daemonp) 'frame 'visible))
  (aw-dispatch-when-more-than 2))

(use-package auto-dark
  :commands (auto-dark-mode auto-dark-toggle-appearance)
  :bind ( :map global-leader-map
          (", T" . auto-dark-toggle-appearance)))

(use-package auto-dim-other-buffers
  :bind ( :map global-leader-map
          ("m D" . auto-dim-other-buffers-mode)))

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
         ("g" . avy-goto-char-timer)))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package cape
  :defer
  ;; Cape provides Completion At Point Extensions
  :init
  ;; #'cape-line ;; Kinda buggy
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ( :map global-leader-map
          ("i c" . claude-code-ide-menu))
  :custom
  (claude-code-ide-debug t)
  (claude-code-ide-use-side-window nil)
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-window-side 'left)
  (claude-code-ide-vterm-render-delay 0.01) ; increase for smoother but less responsive
  (claude-code-ide-terminal-initialization-delay 0.15) ; better render
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
    ("SPC" . consult-project-buffer)
    ("d SPC" . consult-flymake)
    ("n SPC" . consult-org-agenda)
    (", SPC" . consult-emacs-packages)
    (", m" . consult-minor-mode-menu)
    (", s" . consult-emacs)
    (", t" . consult-theme)
    ("x k" . consult-keep-lines)
    ("x f" . flush-lines)
    ("x s" . sort-lines)
    ("x u" . delete-duplicate-lines))
  ( :map minibuffer-mode-map
    ("C-r" . consult-history)
    ("C-M-i" . consult-history))
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
    ("M" . boomark-set-no-overwrite))
  ( :map help-map
    ("SPC" . consult-info))
  :init
  (defun consult-emacs ()
    "Search emacs configuration."
    (interactive)
    (consult-ripgrep user-emacs-directory))
  (defun consult-emacs-packages ()
    "Search emacs configuration."
    (interactive)
    (consult-ripgrep (expand-file-name "lisp" user-emacs-directory) "^(use-package\\ "))
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

(use-package consult-eglot
  :after (eglot consult)
  :bind (:map global-leader-map
              ("l i" . consult-eglot-symbols)))

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

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . read-only-mode))

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
  :bind ( :map search-map
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
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package easy-escape ;; better regexp visuals
  :hook
  ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

(use-package ef-themes
  :defer)

(use-package elfeed
  :commands (elfeed)
  :bind ( :map global-leader-map
          ("o f" . elfeed))
  :custom
  (elfeed-db-directory (expand-file-name "cache/elfeed" user-emacs-directory))
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
  (forge-database-file (expand-file-name "cache/forge/forge-database.sqlite" user-emacs-directory)))

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
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*ghostel\\*" (display-buffer-reuse-mode-window
                                  display-buffer-pop-up-window
                                  display-buffer-at-bottom)))
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
  :config
  (transient-append-suffix 'magit-file-dispatch
    "d" '("T" "Timemachine" git-timemachine))
  (defun git-timemachine-refontify (&rest _)
    "Re-fontify buffer after timemachine revision change."
    (font-lock-ensure))
  (advice-add 'git-timemachine-show-revision :after #'git-timemachine-refontify)
  (with-eval-after-load 'meow
    (defun git-timemachine-toggle-meow-state ()
      "Set meow to motion state when enterint timemachine."
      (if git-timemachine-mode
          (progn
            (meow-motion-mode 1)
            (git-timemachine-refontify))
        (meow-normal-mode 1)))
    (add-hook 'git-timemachine-mode-hook #'git-timemachine-toggle-meow-state)))

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
  (gptel-log-level 'debug)
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((markdown-ts-mode . "### ") (org-mode . "* PROMPT ")))
  (gptel-response-prefix-alist '((org-mode . "** RESPONSE\n")))
  (gptel-gh-token-file (expand-file-name "cache/gptel/copilot-chat/token" user-emacs-directory))
  (gptel-gh-github-token-file (expand-file-name "cache/gptel/copilot-chat/github-token" user-emacs-directory))
  (gptel-crowdsourced-prompts-file (expand-file-name "cache/gptel/crowdsourced-prompts.csv" user-emacs-directory))
  :bind
  ( :map global-leader-map
    ("i SPC" . gptel)
    ("i ," . gptel-menu)
    ("i A" . gptel-add)
    ("i K" . gptel-context-remove-all)
    ("i R" . gptel-rewrite))
  ( :map gptel-mode-map
    ("C-c C-<return>" . gptel-send)
    ("C-c C-c" . gptel-abort)
    ("C-c C-t" . gptel-org-set-topic)
    ("C-c C-p" . gptel-org-set-properties))
  :hook
  (gptel-mode . visual-line-mode)
  (gptel-mode . gptel-highlight-mode)
  :init
  (with-eval-after-load 'dired
    (bind-keys :map dired-mode-map
               ("A" . gptel-add)
               ("K" . gptel-context-remove-all)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*gptel-.*\\*" (display-buffer-reuse-mode-window display-buffer-pop-up-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Copilot\\*" (display-buffer-reuse-mode-window display-buffer-pop-up-window))))

(use-package gptel-aibo
  :bind
  ( :map global-leader-map
    ("i i" . gptel-aibo)
    ("i M-i" . gptel-aibo-complete-at-point))
  ( :map gptel-aibo-mode-map
    ("C-c C-<return>" . gptel-aibo-send))
  :config
  (require 'gptel))

(use-package gptel-magit ;; auto-generate commit messages
  :after magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (require 'gptel)
  (setopt gptel-magit-commit-prompt gptel-magit-prompt-zed))

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
               '("\\*helpful"
                 (display-buffer-reuse-mode-window display-buffer-pop-up-window)
                 (mode . helpful-mode))))

(use-package indent-bars
  :bind
  ( :map global-leader-map
    ("m g" . indent-bars-mode)))

(use-package inheritenv
  :defer
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package jinx
  ;; Dependencies:
  ;; brew install pkgconf enchant hunspell nuspell
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
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
  (meow-keypad-self-insert-undefined nil)
  (meow-expand-hint-remove-delay 2)
  (meow-cursor-type-motion '(hbar . 2))
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
    ;; TODO: Update paren example to actual working mode.
    (setq meow-paren-keymap (make-keymap))
    (meow-define-state paren
      "meow state for interacting with smartparens"
      :lighter " [P]"
      :keymap meow-paren-keymap)
    ;; meow-define-state creates the variable
    (setq meow-cursor-type-paren 'hollow)
    (meow-define-keys 'paren
      '("<escape>" . meow-normal-mode)
      '("l" . sp-forward-sexp)
      '("h" . sp-backward-sexp)
      '("j" . sp-down-sexp)
      '("k" . sp-up-sexp)
      '("n" . sp-forward-slurp-sexp)
      '("b" . sp-forward-barf-sexp)
      '("v" . sp-backward-barf-sexp)
      '("c" . sp-backward-slurp-sexp)
      '("u" . meow-undo))
    (meow-normal-define-key
     (cons "SPC" global-leader-map)
     '("M-DEL" . meow-backward-kill-word)
     '("M-d" . meow-kill-word)
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
     '("-" . negative-argument)
     '("_" . meow-reverse)
     '("(" . meow-start-kmacro)
     '(")" . meow-end-or-call-kmacro)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . nil)
     '("d" . meow-delete)
     '("D" . meow-kill)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-till)
     '("F" . meow-find)
     (cons "g" goto-map)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
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
     '("R" . meow-sync-grab)
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
     '("'" . meow-last-buffer)
     '(";" . meow-comment)
     '(":" . goto-line)
     '("/" . meow-visit)
     '("," . meow-inner-of-thing)
     '("<" . nil)
     '("." . meow-bounds-of-thing)
     '(">" . nil)
     '("<escape>" . meow-cancel-selection)
     '("<backspace>" . meow-backward-delete)))
  :config
  (meow-setup)
  (meow-global-mode))

(use-package meow-tree-sitter
  :after meow
  :commands (meow-tree-sitter-node)
  :init
  (meow-normal-define-key
   '("o" . meow-tree-sitter-node))
  :config
  (meow-tree-sitter-register-defaults))

(use-package orderless
  :custom
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  (completion-styles '(orderless basic)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :after magit
  :config
  (pinentry-start))

(use-package show-font
  :commands (show-font-tabulated))

(use-package simple-modeline
  :demand
  :init
  (defun simple-modeline-segment-branch ()
    "Display current git branch in mode line."
    (when vc-mode
      (let ((branch (truncate-string-to-width vc-mode 40)))
        (propertize (format " %s" branch) 'face 'bold))))
  (defun simple-modeline-segment-project-name ()
    "Display project name in mode line."
    (if-let* ((project (project-current))
              (name (truncate-string-to-width (project-name project) 20)))
        (propertize (format "[%s]" name) 'face 'bold)))
  (defun simple-modeline-segment-buffer-name-2 ()
    "Display buffer's relative-name in mode line."
    (let* ((name (or (relative-file-name) (buffer-file-name) (buffer-name)))
           (shortened-name (string-truncate-left name 50)))
      (propertize (concat "  " shortened-name) 'face 'bold)))
  (defun simple-modeline-segment-spaces ()
    (propertize "  "))
  (defun simple-modeline-segment-misc-info-shortened ()
    (if-let* ((info (simple-modeline-segment-misc-info)))
        (truncate-string-to-width info 20 nil nil "]")))
  :custom
  (simple-modeline-segments
   '(( ;; left indicators
      meow-indicator
      simple-modeline-segment-modified
      simple-modeline-segment-spaces
      simple-modeline-segment-project-name
      ;; simple-modeline-segment-buffer-name
      simple-modeline-segment-buffer-name-2
      simple-modeline-segment-position)
     ( ;; right indicators
      ;; simple-modeline-segment-minor-modes
      ;; simple-modeline-segment-input-method
      ;; simple-modeline-segment-eol
      ;; simple-modeline-segment-encoding
      ;; simple-modeline-segment-vc
      simple-modeline-segment-branch
      ;; simple-modeline-segment-misc-info
      simple-modeline-segment-misc-info-shortened
      simple-modeline-segment-process
      simple-modeline-segment-major-mode
      simple-modeline-segment-spaces)))
  :config
  (simple-modeline-mode))

(use-package tmr
  ;; Dependencies
  ;; brew install ffmpeg
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

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :init
  (defun treesit-fold-auto-enable ()
    "Function to run when a Tree-sitter major mode is activated."
    (when (string-suffix-p "-ts-mode" (symbol-name major-mode))
      (treesit-fold-mode t)))
  :hook
  (after-change-major-mode . treesit-fold-auto-enable))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package visual-replace
  :bind (([remap query-replace] . visual-replace)
         ([remap replace-string] . visual-replace)
         ([remap isearch-query-replace] . visual-replace-from-isearch)
         ([remap isearch-query-replace-regexp] . visual-replace-from-isearch)
         :map search-map
         ("%" . visual-replace-selected)))

(use-package vundo
  :bind ( ("C-x u" . vundo)))

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
