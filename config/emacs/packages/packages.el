;;; -*- lexical-binding: t -*-

(use-package ace-window
  :if (display-graphic-p)
  :bind  (([remap other-window] . ace-window)
          :map goto-map
          ("w 0" . ace-delete-window)
          ("w 1" . ace-delete-other-windows)
          ("w o" . ace-select-window)
          ("w O" . ace-swap-window)))

(use-package agent-shell
  :if (display-graphic-p)
  :bind ( :map global-leader-map
          ("I" . agent-shell))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.50))))

(use-package avy
  :bind (([remap goto-line] . avy-goto-line)
         :map global-leader-map
         ("n r g" . avy-org-refile-as-child)
         ("x g p" . avy-copy-line)
         ("x g P" . avy-copy-region)
         ("x g g" . avy-move-line)
         ("x g G" . avy-move-region)
         ("x g k" . avy-kill-whole-line)
         ("x g K" . avy-kill-region)
         ("x g y" . avy-kill-ring-save-whole-line)
         ("x g Y" . avy-kill-ring-save-region)
         :map isearch-mode-map
         ("M-g" . avy-isearch)
         :map goto-map
         ("G" . avy-resume)
         ("g" . avy-goto-char-timer)))

(use-package beacon
  :if (display-graphic-p) ;; Not pretty in terminal
  :config
  (beacon-mode 1))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :commands (cape-abbrev cape-dict cape-elisp-symbol cape-file cape-history cape-dabbrev cape-line cape-keyword)
  :custom
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-file
         #'cape-dict
         ;; #'cape-elisp-symbol ;; elisp buffers already set its own cape func.
         ;; #'cape-line ;; Kinda buggy
         )))

(use-package consult
  :bind (([remap Info-search] . consult-info)
         ([remap bookmark-jump] . consult-bookmark)
         ;; ([remap goto-line] . consult-goto-line) ;; prefer avy-goto-line
         ([remap imenu] . consult-imenu)
         ([remap keep-lines] . consult-keep-lines)
         ([remap isearch-edit-string] . consult-isearch-history)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap load-theme] . consult-theme)
         ([remap recentf-open] . consult-recent-file)
         ([remap org-search-view] . consult-org-agenda)
         ([remap list-registers] . consult-register)
         ([remap jump-to-register] . consult-register-load)
         ([remap point-to-register] . consult-register-store)
         ([remap keep-lines] . consult-keep-lines)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         :map global-leader-map
         ("k SPC" . consult-flymake)
         :map minibuffer-mode-map
         ("M-i" . consult-history)
         :map search-map
         ("f" . consult-find) ;; works even if not in a project
         ("l" . consult-line)
         ("L" . consult-focus-lines)
         ("s" . consult-ripgrep)
         :map goto-map
         ("I" . consult-imenu-multi)
         ("h" . consult-outline))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer" "SPC"))
    (add-to-list 'project-switch-commands '(consult-ripgrep "Search" "s")))
  :config
  (if (executable-find "fd")
      (bind-keys :map search-map
                 ("f" . consult-fd))))

(use-package consult-eglot
  :after (eglot consult)
  :bind (:map global-leader-map
              ("l i" . consult-eglot-symbols)))

(use-package copilot
  ;; M-x copilot-install-server
  ;; M-x copilot-login
  :if (executable-find "npm")
  :defer 5
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :commands (copilot-mode)
  :bind ( :map copilot-completion-map
          ("M-f" . copilot-accept-completion-by-word)
          ("M-e" . copilot-accept-completion-by-line)
          ("M-<return>" . copilot-accept-completion))
  :custom
  (copilot-idle-delay 0.5)
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t)
  :custom-face
  (copilot-overlay-face
   ((t ( :family "JetBrainsMonoNL Nerd Font Mono"
         :slant italic
         :weight ultra-light
         :inherit completions-annotations))))
  :config
  (add-hook 'prog-mode-hook #'copilot-mode))

(use-package corfu
  :demand
  :if (display-graphic-p)
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo corfu-history corfu-popupinfo))
  ;; When corfu-auto is off, better to not modify bindings.
  :bind ( :map corfu-map
          ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-separator ?\s)
  (corfu-echo-delay 0.2)
  (corfu-popupinfo-delay '(2.0 . 0.5))
  (corfu-min-width 20)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package dashboard
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind ( :map search-map
          ("g" . deadgrep)
          ("G" . rgrep)))

(use-package diff-hl
  ;; TODO: Make it work nice with meow normal
  ;; not really used, better to use magit-diff.
  ;; :bind (:map global-leader-map
  ;;             ("m d" . diff-hl-show-hunk))
  :after magit
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package dimmer
  :if (display-graphic-p) ;; Only works in GUI
  :config
  (dimmer-mode))

(use-package elfeed
  :commands (elfeed)
  :bind ( :map global-leader-map
          ("m f" . elfeed))
  :custom
  (elfeed-feeds
   '(("http://nullprogram.com/feed/" emacs)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://hnrss.org/jobs" jobs)
     ("https://www.reddit.com/r/ExperiencedDevs/top/.rss?t=month" reddit)
     ("https://modern-sql.com/feed" sql)
     ("https://lobste.rs/rss" lobste)
     ("https://hnrss.org/frontpage" hackernews))))

(use-package envrc
  ;; Must activate at the end
  :hook (after-init . envrc-global-mode))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eglot-booster
  ;; cargo install emacs-lsp-booster
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         :map mode-specific-map
         ("." . embark-act)
         (">" . embark-collect)
         ("E" . embark-export)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package git-link
  :bind (:map global-leader-map
              ("x j" . git-link)
              ("x J" . git-link-dispatch)))

(use-package gptel ;; ai, copilot, chatgpt
  ;; llm copilot chat
  ;; Copilot settings:
  ;; (setq gptel-model 'claude-3.7-sonnet)
  ;; (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :custom
  (gptel-default-mode 'org-mode)
  :bind ( :map global-leader-map
          ("i i" . gptel)
          ("i m" . gptel-menu)
          ("i A" . gptel-add)
          ("i K" . gptel-context-remove-all)
          ("i R" . gptel-rewrite)
          :map dired-mode-map
          ("A" . gptel-add)
          ("K" . gptel-context-remove-all))
  :hook
  (gptel-mode . visual-line-mode)
  ;; (gptel-mode . gptel-highlight-mode) ;; Not available for some reason.
  ;; (gptel-post-stream . gptel-auto-scroll) ;; Annoying.
  ;; (gptel-post-response . gptel-beginning-of-response) ;; Doesn't always work.
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* @user: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (with-eval-after-load 'dired-mode
    (bind-keys :map dired-mode-map
               ("I" . gptel-add)))
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c I" . gptel-org-set-topic)
               ("C-c C-<return>" . gptel-send))))

(use-package find-file-in-project
  :bind (:map goto-map
              ("f" . find-file-in-project-at-point)
              ("F" . find-file-in-project-by-selected)))

(use-package forge
  ;; setup:
  ;; Create ~/.authinfo with content:
  ;; machine api.github.com login USERNAME^forge password TOKEN
  ;; where USERNAME: git config --global github.user jubajr17
  ;; and TOKEN: from https://github.com/settings/tokens
  ;;            in a browser to generate a new "classic" token using
  ;;            the repo, user and read:org scopes
  ;; Run M-x auth-source-forget-all-cached
  :commands (forge-dispatch)
  :custom
  ;; password authentication service
  ;; To reload authinfo:
  ;; (auth-source-forget-all-cached)
  (auth-sources '("~/.authinfo")))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("k" . helpful-key) ;; overshadowed by meow
         ("." . helpful-at-point)
         ("F" . helpful-function)))

(use-package imenu-list
  :bind (:map global-leader-map
              ("m i" . imenu-list)
              ("m I" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil)
  :hook
  (imenu-list-mode . hl-line-mode))

(use-package indent-bars
  :bind (:map global-leader-map
              ("m g" . indent-bars-mode))
  :hook
  (yaml-mode . indent-bars-mode))

(use-package magit
  :commands (magit-project-status)
  :bind (:map global-leader-map
              ("j" . magit-file-dispatch)
              ("J" . magit-dispatch))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "j")))
  :custom
  (magit-blame-echo-style 'headings)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-append-suffix 'magit-file-dispatch "G" '("j" "Goto Status Here" magit-status-here)))

(use-package marginalia
  :demand
  :custom
  (completions-detailed nil)
  :config
  (marginalia-mode))

(use-package meow
  :demand
  :custom
  (meow-use-clipboard t)
  (meow-keypad--self-insert-undefined nil)
  (meow-expand-hint-remove-delay 2)
  (meow-cursor-type-motion '(hbar . 2))
  :init
  (defun meow-search-reverse ()
    (interactive)
    (unless (meow--direction-backward-p)
      (meow-reverse))
    (call-interactively #'meow-search))
  (defun meow-setup ()
    (set-face-attribute 'meow-insert-indicator nil :inherit 'bold)
    (set-face-attribute 'meow-beacon-indicator nil :inherit 'bold-italic)
    (set-face-attribute 'meow-motion-indicator nil :inherit 'italic)
    (add-to-list 'meow-expand-exclude-mode-list 'help-mode)
    (meow-motion-define-key
     '("<escape>" . ignore))
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
     '("(" . nil)
     '(")" . nil)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-sync-grab)
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
     ;; '("U" . meow-undo-in-selection)
     '("U" . undo-tree-redo)
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

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package persistent-scratch
  :if (display-graphic-p)
  :defer 5
  :config
  (persistent-scratch-setup-default))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :commands (pinentry-start)
  :init
  (with-eval-after-load 'magit
    (pinentry-start)))

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
      (let ((branch (s-truncate 30 (vc-git--current-branch))))
        (propertize (format " %s" branch) 'face 'font-lock-keyword-face))))
  (defun simple-modeline-segment-project-name ()
    "Display project name in mode line."
    (if (project-current)
        (propertize (format "[%s]" (project-name (project-current))) 'face 'bold)))
  (defun simple-modeline-segment-buffer-name-2 ()
    "Display buffer's relative-name in mode line."
    (propertize (concat "  " (mode-line-buffer-name)) 'face 'mode-line-buffer-id))
  (defun simple-modeline-segment-spaces ()
    (propertize "  "))
  (defun mode-line-buffer-name ()
    (if (buffer-file-name)
        (string-truncate-left (relative-file-name) 50)
      (buffer-name)))
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
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode
      simple-modeline-segment-spaces)))
  :config
  (simple-modeline-mode))

(use-package spacious-padding
  :if (display-graphic-p)
  :bind (:map global-leader-map
              ("m p" . spacious-padding-mode)))

(use-package tmr
  :bind (:map global-leader-map
              ("m t" . tmr-tabulated-view))
  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer tmr-acknowledge-minibuffer))
  :config
  (tmr-mode-line-mode t))

(use-package transient) ;; needed by magit, forge, and others

(use-package undo-tree
  :demand
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo-tree-history"))))
  :config
  (global-undo-tree-mode 1))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package vertico-posframe
  :if (display-graphic-p)
  :defer
  :after vertico
  :bind (:map global-leader-map
              ("m v" . vertico-posframe-mode))
  :custom
  (vertico-posframe-min-width 80))

(use-package visual-replace
  :demand
  :bind ( :map search-map
          ("%" . visual-replace-selected))
  :config
  (visual-replace-global-mode))

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
  :demand
  :bind (:map goto-map
              ("&" . yas-visit-snippet-file)
              :map global-leader-map
              ("x &" . yas-new-snippet))
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode))
