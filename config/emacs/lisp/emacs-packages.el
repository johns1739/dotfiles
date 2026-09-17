;;; emacs-packages.el --- Third Party Packages  -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :demand
  :if (or (daemonp) (and (memq window-system '(mac ns x)) (display-graphic-p)))
  :custom
  ;; (exec-path-from-shell-debug t)
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package ace-window
  :if (display-graphic-p) ;; Num assignments in terminal are incosistent.
  :bind  (([remap other-window] . ace-window)
          :map goto-map
          ("w 0" . ace-delete-window)
          ("w 1" . ace-delete-other-windows)
          ("w o" . ace-select-window)
          ("w O" . ace-swap-window))
  :custom
  (aw-dispatch-when-more-than 2))

(use-package agent-shell
  :disabled ;; prefer gptel-agent
  ;; https://github.com/xenodium/agent-shell
  :if (display-graphic-p)
  :commands (agent-shell)
  :init
  (with-eval-after-load 'project
    (project-add-switch-command 'agent-shell "Agent" "I"))
  :bind ( :map global-leader-map
          ("I" . agent-shell))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.5))))

(use-package aidermacs
  :disabled ;; too expensive, requires python dependency aider
  :if (and (display-graphic-p) (executable-find "aider"))
  :bind ( :map global-leader-map
          ("a" . aidermacs-transient-menu))
  :custom
  ;; (aidermacs-default-model "gpt-5.2")
  ;; (aidermacs-default-model "gemini-2.5-pro"))
  (aidermacs-default-chat-mode 'architect))

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

(use-package casual ;; Better transient menu
  :disabled ;; Too much configuration for different modes.
  :bind ( :map org-agenda-mode-map
          ("C-o" . casual-agenda-tmenu)))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ( :map global-leader-map
          ("i c" . claude-code-ide-menu))
  :custom
  (claude-code-ide-debug t)
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-window-side 'left)
  (claude-code-ide-vterm-render-delay 0.01) ; increase for smoother but less responsive
  (claude-code-ide-terminal-initialization-delay 0.15) ; better render
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package command-log-mode
  :disabled ;; Use C-h l
  :bind
  ( :map global-leader-map
    ("m l" . clm/toggle-command-log-buffer))
  :config
  (global-command-log-mode))

(use-package consult
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap isearch-edit-string] . consult-isearch-history)
   ([remap recentf-open] . consult-recent-file)
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
    ("j". consult-register)
    ("i" . consult-imenu)
    ("I" . consult-imenu-multi)
    ("l" . consult-line)
    ("L" . consult-line-multi)
    ("r" . consult-recent-file)
    ("s" . consult-ripgrep)
    ("k" . consult-focus-lines)
    ("m" . consult-mark))
  ( :map goto-map
    ("SPC" . consult-buffer)
    ("o" . consult-outline)
    ("j" . consult-register-load)
    ("J" . consult-register-store)
    ("m" . consult-bookmark))
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

(use-package consult-denote ;; Prot's note-taking with org
  :disabled ;; not using denote
  :bind (:map global-leader-map
              ("n d f" . consult-denote-find)
              ("n d s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-eglot
  :after (eglot consult)
  :bind (:map global-leader-map
              ("l i" . consult-eglot-symbols)))

(use-package consult-flycheck
  :disabled ;; doesn't work well with (rails) bundle commands, e.g. bundle exec rubocop
  :after consult
  :bind
  ( :map global-leader-map
    ("d SPC" . consult-flycheck))
  :config
  (require 'flycheck))

(use-package consult-ghostel
  :vc ( :url "https://github.com/dakra/ghostel"
        :lisp-dir "extensions/consult-ghostel"
        :rev :newest)
  :after (consult ghostel)
  :bind
  ( :map global-leader-map
    ("k SPC" . consult-ghostel)))

(use-package copilot
  ;; M-x copilot-install-server
  ;; M-x copilot-login
  :if (executable-find "npm")
  :commands (copilot-mode)
  :bind ( :map global-leader-map
          ("i o" . copilot-mode)
          ("i O" . global-copilot-mode)
          :map copilot-completion-map
          ("M-f" . copilot-accept-completion-by-word)
          ("M-e" . copilot-accept-completion-by-line)
          ("M-TAB" . copilot-accept-completion)
          ("M-RET" . copilot-accept-completion))
  :custom
  (copilot-idle-delay 0.5)
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t)
  (copilot-install-dir (expand-file-name "cache/copilot/" user-emacs-directory))
  :custom-face
  (copilot-overlay-face
   ((t ( :family "JetBrainsMonoNL Nerd Font Mono"
         :slant italic
         :weight ultra-light
         :inherit completions-annotations)))))

(use-package copilot-chat
  :disabled ;; too slow, better to use gptel
  :if (display-graphic-p)
  :after (request org markdown-mode copilot))

(use-package corfu
  :defer 1
  :bind ( :map corfu-map
          ("TAB" . corfu-complete)
          ("M-TAB" . corfu-expand)
          ("RET" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-echo-delay 0.3)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(0.6 . 0.3))
  (corfu-preselect 'prompt)
  (corfu-preview-current 'prompt)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
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
  :unless (display-graphic-p) ;; Better when used w/ emacs server.
  :custom
  (initial-buffer-choice 'dashboard-open)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind ( :map search-map
          ("g" . deadgrep)
          ("G" . rgrep)
          :map deadgrep-mode-map
          ("C-w" . deadgrep-edit-mode)))

(use-package denote ;; used to create references to org notes
  :bind ( :map global-leader-map
          ("n n" . denote-open-or-create)
          ("n N" . denote-region))
  :custom
  (denote-directory "~/Documents/notes/refs")
  (denote-date-prompt-use-org-read-date t)
  :init
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c n l" . denote-link-or-create)
               ("C-c n /" . denote-find-link)
               ("C-c n ?" . denote-find-backlink)
               ("C-c n r" . denote-rename-file)))
  :config
  (denote-rename-buffer-mode))

(use-package denote-journal
  :bind ( :map global-leader-map
          ("n j" . denote-journal-new-or-existing-entry))
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  (setq denote-journal-directory denote-directory)
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

(use-package devdocs
  :disabled ;; clunky and difficult to keep updated.
  :bind (:map global-leader-map
              ("m h I" . devdocs-install)
              ("m h h" . devdocs-lookup)
              ("m h s" . devdocs-search)))

(use-package diff-hl ;; git diff changes in fringe
  :after magit
  :commands (diff-hl-show-hunk)
  :init
  (defun diff-hl-toggle-meow-state ()
    (if diff-hl-show-hunk-mode
        (meow-motion-mode 1)
      (meow-normal-mode 1)))
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-file-dispatch "d" '("." "show-diff-hunk" diff-hl-show-hunk)))
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (with-eval-after-load 'meow
    (add-hook 'diff-hl-show-hunk-mode-hook #'diff-hl-toggle-meow-state))
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package dimmer
  :disabled ;; Fails to install with latest revision. Use auto-dim-other-buffers
  :if (display-graphic-p) ;; Only works in GUI
  :config
  (dimmer-mode t))

(use-package dired-subtree
  :disabled ;; github repo setup strange
  :vc (:url "https://github.com/Fuco1/dired-hacks")
  :init
  (with-eval-after-load 'dired-mode
    (require 'dired-subtree))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package docker
  :if (executable-find "docker")
  :bind (:map global-leader-map
              ("o k" . docker))
  :config
  (let ((column (seq-find (lambda (col) (equal (plist-get col :name) "Image"))
                          docker-container-columns)))
    (plist-put column :width 62)))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package easy-escape
  :hook
  ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

(use-package eat
  :disabled ;; prefer vterm
  ;; When eat-terminal input is acting weird, try re-compiling with command:
  ;; (eat-compile-terminfo)
  :bind ( :map global-leader-map
          ("k t" . eat-project)
          ("k T" . eat)
          :map eat-semi-char-mode-map
          ("M-o" . other-window))
  :init
  (with-eval-after-load 'project
    (project-add-switch-command 'eat-project "Eat" "t"))
  :custom
  (eat-enable-auto-line-mode nil) ;; more intuitive to use semi-char mode
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (process-adaptive-read-buffering t)
  :hook
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-load . eat-eshell-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eat\\*" (display-buffer-reuse-mode-window display-buffer-pop-up-window))))

(use-package eglot-booster
  :disabled ;; eglot-booster not available melpa?
  ;; cargo install emacs-lsp-booster
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package eldoc-box
  :disabled ;; annoying GUI
  :if (display-graphic-p) ;; Only available on GUI
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

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

(use-package ellama
  :disabled ;; prefer gptel
  :custom
  (ellama-user-nick "Lobo")
  (ellama-assistant-nick "Cody")
  (ellama-language "English")
  (ellama-spinner-enabled t)
  ;; (ellama-chat-display-action-function #'display-buffer-full-frame)
  ;; (ellama-instant-display-action-function #'display-buffer-at-bottom)
  (ellama-keymap-prefix "C-;")
  (ellama-auto-scroll t)
  :hook
  (org-ctrl-c-ctrl-c . ellama-chat-send-last-message)
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama :chat-model "qwen2.5:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama :chat-model "qwen2.5-coder:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-summarization-provider
          (make-llm-ollama :chat-model "qwen2.5-coder:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (ellama-context-header-line-global-mode 1))

(use-package elysium
  :disabled ;; doesn't work very well, buggy. Prefer aibo.
  :after (gptel)
  :custom
  (elysium-window-size 0.5)
  (elysium-window-style 'vertical)
  :bind (:map global-leader-map
              ("i ." . elysium-query)
              ("i >" . elysium-add-context)
              ("i ," . elysium-toggle-window)
              ("i <" . elysium-clear-buffer))
  :hook
  (elysium-apply-changes . smerge-mode))

(use-package embark
  :disabled ;; rarely used
  :bind (([remap describe-bindings] . embark-bindings)
         :map ctl-x-map
         ("A" . embark-act)
         ("C" . embark-collect)
         ("E" . embark-export)))

(use-package envrc
  ;; Must activate at the end
  :hook (after-init . envrc-global-mode))

(use-package embark-consult
  :disabled
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package flycheck
  :disabled ;; unable to call (rails) bundle exec rubocop
  :commands (global-flycheck-mode flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-fringe)
  ;; :hook
  ;; (flycheck-mode . flycheck-set-indication-mode)
  :bind
  ( :map global-leader-map
    ("D" . flycheck-mode)
    ("d SPC" . nil) ;; unset a flymake binding
    ("d ." . flycheck-explain-error-at-point)
    ("d c" . flycheck-compile)
    ("d d" . flycheck-list-errors)
    ("d D" . flycheck-buffer)
    ("d p" . nil) ;; unset a flymake binding
    ("d v" . flycheck-verify-setup)
    ("d y" . flycheck-copy-errors-as-kill)))

(use-package flycheck-eglot
  :disabled ;; flycheck does not work well with rails (eg bundle exec rubocop)
  :after eglot
  :init
  (require 'flycheck)
  (require 'consult-flycheck)
  :config
  (global-flycheck-eglot-mode t))

(use-package find-file-in-project
  :bind
  ( :map goto-map
    ("u" . find-file-in-project-at-point)
    ("U" . find-file-at-point))
  ( :map project-prefix-map
    ("u" . find-file-in-project-at-point)
    ("U" . find-file-in-project-by-selected)))

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
  (with-eval-after-load 'project
    (project-add-switch-command #'ghostel-project "Ghostel" "t")))

(use-package git-link
  :commands (git-link git-link-dispatch)
  :bind
  ( :map global-leader-map
    ("y j" . git-link)
    ("y J" . git-link-dispatch))
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-file-dispatch "e" '("y" "Copy Link" git-link))
    (transient-append-suffix 'magit-file-dispatch "y" '("Y" "Copy Link Dispatch" git-link-dispatch))))

(use-package git-modes
  :disabled) ;; Long load time.

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-file-dispatch "d" '("T" "Timemachine" git-timemachine)))
  :config
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

(use-package gptel-agent
  :vc ( :url "https://github.com/karthink/gptel-agent" :rev :newest)
  :bind
  ( :map global-leader-map
    ("i a" . gptel-agent))
  ( :map project-prefix-map
    ("i" . gptel-agent))
  :config
  (require 'gptel)
  (gptel-agent-update))

(use-package gptel-aibo
  :bind
  ( :map global-leader-map
    ("i i" . gptel-aibo)
    ("i I" . gptel-aibo-complete-at-point))
  ( :map gptel-aibo-mode-map
    ("C-c C-<return>" . gptel-aibo-send))
  :config
  (require 'gptel))

(use-package gptel-commit
  :disabled ;; prefer gptel-magit
  :after  magit
  :custom
  (gptel-commit-stream t)
  :config
  (require 'gptel)
  (with-eval-after-load 'magit
    (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
    (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale)))

(use-package gptel-magit ;; auto-generate commit messages
  :after magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (require 'gptel)
  (setopt gptel-magit-commit-prompt gptel-magit-prompt-zed))

(use-package gptel-prompts
  :disabled ;; Fails to install, package not available
  :demand
  :after (gptel)
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

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

(use-package imenu-list
  :bind (:map global-leader-map
              ("o I" . imenu-list)
              ("o i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil)
  :hook
  (imenu-list-mode . hl-line-mode))

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

;; https://github.com/unmonoqueteclea/jira.el?tab=readme-ov-file#authentication
(use-package jira
  :disabled ;; jira issue not loading list
  :after (request)
  :defer)

(use-package keychain-environment
  :if (eq system-type 'darwin) ;; macos
  :config
  (keychain-refresh-environment))

(use-package kirigami
  :bind
  ( :map global-leader-map
    ("z F" . kirigami-close-folds)
    ("z O" . kirigami-open-folds)
    ("z f" . kirigami-close-fold)
    ("z o" . kirigami-open-fold)
    ("z z" . kirigami-toggle-fold)
    ("z ." . kirigami-open-fold-rec)))

(use-package kubernetes
  :if (and (display-graphic-p) (executable-find "kubectl"))
  :commands (kubernetes-overview)
  :bind (:map global-leader-map
              ("o K" . kubernetes-overview))
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

(use-package ligature
  :disabled ;; not easy to setup
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
                                       "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
                                       "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
                                       "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>" "<* *>" "</" "</>" "/>" "<!--"
                                       "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
                                       "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-"
                                       "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
                                       "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::"
                                       ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))

(use-package lsp-mode
  :disabled ;; preferred eglot
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-format-buffer] . lsp-format-buffer)
               ([remap xref-find-references] . lsp-find-references)
               ([remap eldoc] . lsp-describe-thing-at-point)))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-set-bindings))

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

(use-package magit-delta
  :disabled ;; diff colors are difficult to see, ugly
  ;; Dependencies
  ;; brew install git-delta
  :if (and (display-graphic-p) (executable-find "delta"))
  :hook (magit-mode . magit-delta-mode))

(use-package magit-todos
  :disabled ;; slow startup if project too big, sometimes list is missing
  :after magit
  :config
  (magit-todos-mode 1))

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
     '("f" . meow-find)
     '("F" . nil)
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
     '("t" . meow-till)
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

(use-package ob-http
  :disabled ;; Better to use curl in org-source blocks.
  :after org)

(use-package ob-restclient
  :after org)

(use-package orderless
  :custom
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  (completion-styles '(orderless basic)))

(use-package org-mcp
  ;; Register with claude:
  ;; claude mcp add -s user -t stdio org-mcp -- ~/.config/emacs/emacs-mcp-stdio.sh --server-id=org-mcp --init-function=org-mcp-enable --stop-function=org-mcp-disable
  :defer t ;; NOTE: Must be required to call (org-mcp-enable)
  :custom
  (org-mcp-allowed-files '("~/Documents/notes/tasks.org")))

(use-package org-modern ;; Better look for org
  :disabled ;; still in its early stages.
  :after org
  :custom-face
  ;; (org-modern-priority ((t (:inverse-video nil))))
  ;; (org-modern-todo ((t (:inverse-video nil))))
  (org-modern-done ((t (:foreground nil :background nil :inherit (org-done org-modern-label)))))
  :custom
  (org-modern-todo-faces
   '(("WIP" :foreground "spring green" :inherit org-modern-todo)
     ("ACTIVE" :foreground "spring green" :inherit org-modern-todo)
     ("REVIEW" :foreground "spring green" :inherit org-modern-todo)
     ("BACKLOG" :inherit org-modern-done)))
  :config
  (global-org-modern-mode))

(use-package org-remark
  :disabled ;; does not work smoothly as expected
  :after org)

(use-package org-roam
  :disabled ;; never really used, denote simpler and easier to understand
  :commands (org-roam-node-find)
  :init
  (defun org-roam-setup-directory ()
    (setopt org-roam-directory (expand-file-name "org-roam/" org-directory))
    (make-directory org-roam-directory t)
    (org-roam-db-autosync-mode))
  :bind
  ( :map global-leader-map
    ("n n" . org-roam-node-find)
    ("n N" . org-roam-capture)
    :map mode-specific-map
    ("n a a" . org-roam-dailies-goto-today)
    ("n a y" . org-roam-dailies-goto-yesterday)
    ("n a t" . org-roam-dailies-goto-tomorrow)
    ("n n" . org-roam-node-find)
    ("n N" . org-roam-capture)
    :map org-mode-map
    ("C-c n i" . org-roam-node-insert)
    ("C-c n k" . org-roam-extract-subtree)
    ("C-c n ;" . org-roam-alias-add)
    ("C-c n :" . org-roam-tag-add)
    ("C-c n w" . org-roam-refile))
  :custom
  (org-roam-db-location (expand-file-name "cache/org-roam/org-roam.db" user-emacs-directory))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:12}" 'face 'org-tag)))
  :config
  (org-roam-setup-directory))

(use-package paredit
  :disabled ;; auto formats that conflicts with lang's formatting.
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package persistent-scratch
  :disabled ;; not really used, slower start-up
  :if (display-graphic-p)
  :config
  (persistent-scratch-setup-default))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :after magit
  :config
  (pinentry-start))

(use-package popper
  :disabled ;; prefer display-buffer-alist
  :demand
  :if (display-graphic-p)
  :bind ( :map popper-mode-map
          ("M-`" . popper-cycle))
  :custom
  (popper-reference-buffers
   '("^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$"  shell-mode
     "^\\*term.*\\*$"   term-mode
     "^\\*vterm.*\\*$"  vterm-mode
     "^\\*eat.*\\*$"  eat-mode
     ))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

;; http request library
(use-package request ;; Jira dependency
  :disabled
  :after jira
  :defer
  :custom
  (request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(use-package show-font
  :if (display-graphic-p) ;; none exist in terminal
  :bind (:map global-leader-map
              (", X" . show-font-tabulated)))

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

(use-package spacious-padding
  :if (display-graphic-p) ;; fails to add padd in terminal
  :bind ( :map global-leader-map
          ("m P" . spacious-padding-mode)))

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

(use-package trashed
  :disabled ;; Never used.
  :bind (:map global-leader-map
              ("m _" . trashed))
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treemacs
  :disabled ;; Prefer builtin dired.
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map global-leader-map
              ("m p" . treemacs-select-window)
              ("m P" . treemacs))
  :custom
  (treemacs-no-png-images t)
  (treemacs-hide-dot-git-directory t)
  :config
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t))

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :init
  (defun treesit-fold-auto-enable ()
    "Function to run when a Tree-sitter major mode is activated."
    (when (string-suffix-p "-ts-mode" (symbol-name major-mode))
      (treesit-fold-mode t)))
  :hook
  (after-change-major-mode . treesit-fold-auto-enable))

(use-package undo-tree
  :disabled ;; trying out vundo, works with native undo
  :demand
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "cache/undo-tree-history/" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package vertico-posframe
  :if (display-graphic-p) ;; does not work in terminal
  :after vertico
  :bind ( :map global-leader-map
          ("m V" . vertico-posframe-mode))
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-min-width 80))


(use-package visual-fill-column
  ;; https://codeberg.org/joostkremers/visual-fill-column
  :defer
  :custom
  (visual-fill-column-center-text t)
  :init
  (defun visual-fill-column-setup ()
    (display-line-numbers-mode -1))
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-setup))

(use-package visual-replace
  :bind (([remap query-replace] . visual-replace)
         ([remap replace-string] . visual-replace)
         ([remap isearch-query-replace] . visual-replace-from-isearch)
         ([remap isearch-query-replace-regexp] . visual-replace-from-isearch)
         :map search-map
         ("%" . visual-replace-selected)))

(use-package vterm
  :disabled ;; prefer ghostel
  ;; Dependencies (linux):
  ;; sudo apt update
  ;; sudo apt install libtool libtool-bin
  :bind ( :map global-leader-map
          ("k T" . vterm)
          ("k t" . vterm-project))
  :init
  (defun vterm-project ()
    (interactive)
    (require 'vterm) ;; Prevent defining as dynamic an already lexical var: vterm-buffer-name
    (let ((vterm-buffer-name
           (or (and (project-current)
                    (format "*%s-vterm*" (project-name (project-current))))
               "*project-vterm*"))
          (default-directory (or (project-directory) default-directory)))
      (vterm)))
  :custom
  (vterm-always-compile-module t)
  (vterm-copy-exclude-prompt t)
  (vterm-kill-buffer-on-exit t)
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000) ;; can't go higher than this
  :config
  (with-eval-after-load 'project
    (project-add-switch-command 'vterm-project "vTerm" "t"))
  (add-to-list 'display-buffer-alist
               '("\\*.*vterm\\*" (display-buffer-reuse-mode-window display-buffer-pop-up-window))))

(use-package vundo
  :bind ( ("C-x u" . vundo)))

(use-package whisper ;; Audio recording
  :disabled ;; could be better
  :if (executable-find "ffmpeg")
  :bind (("C-M-y" . whisper-run))
  :init
  (defun rk/get-ffmpeg-device ()
    "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
    (unless (string-equal system-type "darwin")
      (error "This function is currently only supported on macOS"))
    (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
      (cl-loop with at-video-devices = nil
               with at-audio-devices = nil
               with video-devices = nil
               with audio-devices = nil
               for line in lines
               when (string-match "AVFoundation video devices:" line)
               do (setq at-video-devices t
                        at-audio-devices nil)
               when (string-match "AVFoundation audio devices:" line)
               do (setq at-audio-devices t
                        at-video-devices nil)
               when (and at-video-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
               when (and at-audio-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
               finally return (list (nreverse video-devices) (nreverse audio-devices)))))
  (defun rk/find-device-matching (string type)
    "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
    (let* ((devices (rk/get-ffmpeg-device))
           (device-list (if (eq type :video)
                            (car devices)
                          (cadr devices))))
      (cl-loop for device in device-list
               when (string-match-p string (cdr device))
               return (car device))))
  (defcustom rk/default-audio-device nil
    "The default audio device to use for whisper.el and outher audio processes."
    :type 'string)
  (defun rk/select-default-audio-device (&optional device-name)
    "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
    (interactive)
    (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
           (indexes (mapcar #'car audio-devices))
           (names (mapcar #'cdr audio-devices))
           (name (or device-name (completing-read "Select audio device: " names nil t))))
      (setq rk/default-audio-device (rk/find-device-matching name :audio))
      (when (boundp 'whisper--ffmpeg-input-device)
        (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device))))))

(use-package writeroom-mode
  :if (display-graphic-p)
  :bind ( :map global-leader-map
          ("m w" . writeroom-mode)
          ("m W" . global-writeroom-mode))
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width 120))

(use-package xclip
  :demand
  :unless (display-graphic-p)
  :config
  (xclip-mode))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :defer 1
  :bind ( :map goto-map
          ("&" . yas-visit-snippet-file)
          :map global-leader-map
          ("x &" . yas-new-snippet))
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :disabled ;; Better to rely on custom built templates over externals.
  :after yasnippet)

(provide 'emacs-packages)
;;; emacs-packages.el ends here
