;;; emacs-disabled.el --- Disabled Third Party Packages  -*- lexical-binding: t; -*-

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

(use-package casual ;; Better transient menu
  :disabled ;; Too much configuration for different modes.
  :bind ( :map org-agenda-mode-map
          ("C-o" . casual-agenda-tmenu)))

(use-package command-log-mode
  :disabled ;; Use C-h l instead.
  :bind
  ( :map global-leader-map
    ("m l" . clm/toggle-command-log-buffer))
  :config
  (global-command-log-mode))

(use-package consult-denote ;; Prot's note-taking with org
  :disabled ;; not using denote
  :bind (:map global-leader-map
              ("n d f" . consult-denote-find)
              ("n d s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-flycheck
  :disabled ;; doesn't work well with (rails) bundle commands, e.g. bundle exec rubocop
  :after consult
  :bind
  ( :map global-leader-map
    ("d SPC" . consult-flycheck))
  :config
  (require 'flycheck))

(use-package consult-gh
  :disabled ;; not very useful
  :if (executable-find "gh")
  :after (consult)
  :defer
  :custom
  (consult-gh-default-clone-directory "~/projects")
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-default-interactive-command #'consult-gh-search-repos)
  (consult-gh-group-dashboard-by :type)
  (consult-gh-preview-major-mode 'gfm-view-mode)
  :config
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  (require 'markdown-mode)
  (require 'yaml)
  (consult-gh-enable-default-keybindings))

(use-package copilot
  :disabled ;; GPTEL / Claude tools are better.
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

(use-package denote ;; used to create references to org notes
  :disabled ;; rarely used.
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
               ("C-c n r" . denote-rename-file)
               ("C-c n R" . denote-change-file-type-and-front-matter)))
  :config
  (denote-rename-buffer-mode))

(use-package denote-journal
  :disabled ;; not using denote
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
  :disabled ;; prefer terminal
  :if (executable-find "docker")
  :bind (:map global-leader-map
              ("o k" . docker))
  :config
  (let ((column (seq-find (lambda (col) (equal (plist-get col :name) "Image"))
                          docker-container-columns)))
    (plist-put column :width 62)))

(use-package eat
  :disabled ;; prefer ghostel
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

(use-package embark-consult
  :disabled
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package git-modes
  :disabled) ;; Long load time.

(use-package gptel-agent
  :disabled ;; prerfer aibo
  :vc ( :url "https://github.com/karthink/gptel-agent" :rev :newest)
  :bind
  ( :map global-leader-map
    ("i a" . gptel-agent))
  ( :map project-prefix-map
    ("i" . gptel-agent))
  :config
  (require 'gptel)
  (gptel-agent-update))

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

(use-package gptel-prompts
  :disabled ;; Fails to install, package not available
  :demand
  :after (gptel)
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(use-package imenu-list
  :disabled ;; rarely used
  :bind (:map global-leader-map
              ("o I" . imenu-list)
              ("o i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil)
  :hook
  (imenu-list-mode . hl-line-mode))

;; https://github.com/unmonoqueteclea/jira.el?tab=readme-ov-file#authentication
(use-package jira
  :disabled ;; jira issue not loading list
  :after (request)
  :defer)

(use-package kirigami
  :disabled ;; rarely used
  :bind
  ( :map global-leader-map
    ("z F" . kirigami-close-folds)
    ("z O" . kirigami-open-folds)
    ("z f" . kirigami-close-fold)
    ("z o" . kirigami-open-fold)
    ("z z" . kirigami-toggle-fold)
    ("z ." . kirigami-open-fold-rec)))

(use-package kubernetes
  :disabled ;; prefer command line
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

(use-package magit-difftastic ;; better side-to-side diff view
  :disabled ;; prefer traditional vs side-to-side
  ;; brew install difftastic
  :if (and (display-graphic-p) (executable-find "difft"))
  :vc (:url "https://github.com/rschmukler/magit-difftastic" :rev :newest)
  :after magit)

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

(use-package ob-http
  :disabled ;; Better to use curl in org-source blocks.
  :after org)

(use-package ob-restclient
  :disabled ;; Better to use curl
  :after org)

(use-package org-mcp
  :disabled ;; no longer required
  ;; Register with claude:
  ;; claude mcp add -s user -t stdio org-mcp -- ~/.config/emacs/emacs-mcp-stdio.sh --server-id=org-mcp --init-function=org-mcp-enable --stop-function=org-mcp-disable
  :commands (org-mcp-enable)
  :custom
  (org-mcp-allowed-files '("~/Documents/notes/tasks.org"))
  :config
  (require 'org))

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

(use-package persistent-scratch
  :disabled ;; not really used, slower start-up
  :if (display-graphic-p)
  :config
  (persistent-scratch-setup-default))

(use-package pg
  :disabled ;; dependency no longer required
  :defer)

(use-package pgmacs
  :disabled ;; lags on some actions
  :vc (:url "https://github.com/emarsden/pgmacs" :rev :newest)
  :defer
  :requires (pg))

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

(use-package spacious-padding
  :disabled
  :bind ( :map global-leader-map
          ("m P" . spacious-padding-mode)))

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

(use-package undo-tree
  :disabled ;; trying out vundo, works with native undo
  :demand
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "cache/undo-tree-history/" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))

(use-package vertico-posframe
  :disabled ;; gets in the way
  :if (display-graphic-p) ;; does not work in terminal
  :after vertico
  :bind ( :map global-leader-map
          ("m V" . vertico-posframe-mode))
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-min-width 80))

(use-package visual-fill-column
  :disabled ;; Conflicts & hides diff-hl's margins
  ;; https://codeberg.org/joostkremers/visual-fill-column
  :defer
  :custom
  (visual-fill-column-center-text t)
  :init
  (defun visual-fill-column-setup ()
    (display-line-numbers-mode -1))
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-setup))

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

(use-package yasnippet-snippets
  :disabled ;; Better to rely on custom built templates over externals.
  :after yasnippet)

(provide 'emacs-disabled)
;;; emacs-disabled.el ends here
