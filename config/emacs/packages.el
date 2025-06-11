;;; -*- lexical-binding: t -*-

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
(setq package-install-upgrade-built-in t)

;;;; Packages

(use-package ace-window
  :bind  ([remap other-window] . ace-window))

(use-package avy
  :bind (:map goto-map
              ("g" . avy-goto-char-2)
              ("l" . avy-goto-line)
              ("a k" . avy-kill-whole-line)
              ("a K" . avy-kill-region)
              ("a m" . avy-move-line)
              ("a M" . avy-move-region)
              ("a y" . avy-copy-line)
              ("a Y" . avy-copy-region)))

(use-package beacon
  :if (display-graphic-p)
  :config
  (beacon-mode 1))

(use-package cc-mode
  :mode "\\.c\\'"
  :ensure nil
  :straight nil
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(cc-mode . c-ts-mode)))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :bind (("M-l" . cape-line)
         :map global-leader-map
         ("i d" . cape-dict)
         ("i e" . cape-elisp-symbol)
         ("i f" . cape-file)
         ("i h" . cape-history)
         ("i i" . cape-dabbrev)
         ("i l" . cape-line)
         ("i s" . cape-keyword))
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

(use-package color-theme-sanityinc-tomorrow
  :defer t)

(use-package common-lisp-mode
  :straight nil
  :ensure nil
  :mode
  (("\\.lisp$" . common-lisp-mode)
   ("\\.clisp$" . common-lisp-mode))
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el") t) ;; t = noerror
  (setq inferior-lisp-program "sbcl"))

(use-package consult
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :bind (([remap Info-search] . consult-info)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
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
         ([remap project-find-regexp] . consult-ripgrep)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake) ;; Errors aren't verbose sometimes
         :map global-leader-map
         ("k SPC" . consult-compile-error)
         :map search-map
         ("l" . consult-line)
         ("L" . consult-focus-lines)
         :map goto-map
         ("I" . consult-imenu-multi)
         ("o" . consult-outline))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  (add-to-list 'project-switch-commands '(consult-ripgrep "Search" "s")))

(use-package consult-denote
  :disabled
  :bind (:map global-leader-map
              ("N f" . consult-denote-find)
              ("N s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-flycheck
  :commands (consult-flycheck))

(use-package copilot
  :disabled ;; requires copilot subscription token
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :after corfu
  :bind (:map copilot-completion-map
              ("M-f" . copilot-accept-completion-by-word)
              ("C-e" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("C-<tab>" . copilot-accept-completion))
  :custom
  (corfu-auto nil)
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay 0.5)
  :custom-face
  (copilot-overlay-face ((t (:family "Monaspace Krypton" :slant 'italic))))
  :hook
  (prog-mode . copilot-mode))

(use-package corfu
  :demand
  :if (display-graphic-p)
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo corfu-history corfu-popupinfo))
  :bind (:map corfu-map
              ("TAB" . nil)
              ("RET" . nil)
              ("SPC" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-separator ?\s)
  (corfu-echo-delay 0.2)
  (corfu-popupinfo-delay '(1.25 . 0.2))
  (corfu-min-width 20)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2))

(use-package dashboard
  :disabled
  :demand
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind (:map search-map ("g" . deadgrep)))

(use-package denote
  :disabled
  :bind (:map global-leader-map
              ("N SPC" . denote-open-or-create)
              ("N n" . denote)
              ("N j" . denote-journal-extras-new-or-existing-entry)
              ("N l" . denote-link-or-create)
              ("N k" . denote-find-link)
              ("N K" . denote-find-backlink)
              ("N r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory "~/workspaces/notes")
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode))

(use-package diff-hl
  :bind (:map global-leader-map
              ("j ," . diff-hl-show-hunk))
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package dired-subtree
  :init
  (defun dired-subtree-setup ()
    (require 'dired-subtree))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :hook
  (dired-mode . dired-subtree-setup)
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eat
  ;; When eat-terminal input is acting weird, try re-compiling with command:
  ;; (eat-compile-terminfo)
  :commands (eat eat-project eat-other-window)
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind ((([remap eshell] . eat)
          ([remap project-eshell] . eat-project)))
  :custom
  (eat-term-scrollback-size nil)
  (process-adaptive-read-buffering nil) ;; possible perf improvement
  (read-process-output-max (* 4 1024 1024)) ;; 4MB
  (eat-kill-buffer-on-exit t)
  :init
  (defun eat-mode-setup ()
    (display-line-numbers-mode -1))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(eat-project "Terminal" "t")))
  :hook
  (eat-mode . eat-mode-setup))

(use-package ef-themes
  :defer t)

(use-package eglot
  :straight nil
  :bind (:map global-leader-map
              ("l TAB" . eglot-format-buffer)
              ("l l" . eglot)
              ("l L" . eglot-reconnect)
              ("l q" . eglot-shutdown)
              ("l Q" . eglot-shutdown-all)
              ("l r" . eglot-rename)
              ("l a" . eglot-code-actions)))

(use-package eglot-booster
  ;; cargo install emacs-lsp-booster
  :after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package eldoc-box
  :disabled ;; Annoying
  :if (display-graphic-p)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

(use-package elixir-ts-mode
  :mode (("\\.ex$" . elixir-ts-mode)
         ("\\.exs$" . elixir-ts-mode)
         ("\\.heex$" . heex-ts-mode))
  :init
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    (add-to-list 'compilation-error-regexp-alist 'elixir-warning-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-warning-target
                   "└─ \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3 1 1)))
  (defun elixir-compile ()
    (interactive)
    (setq compile-command
          (cond ((string-match-p "_test.exs\\'" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (relative-file-name)))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list "mix test " file-name))
                     (string-join (list "mix test " (format "%s:%s" file-name linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))
  (defun elixir-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'elixir-compile))
  (defun elixir-setup ()
    (setq outline-regexp " *\\(describe \\|test \\|setup \\)")
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . elixir-compile)
               ([remap comint-dwim] . elixir-comint)))
  :hook
  (elixir-ts-mode . elixir-setup)
  :config
  ;; TODO: Enable more features
  ;; https://joaotavora.github.io/eglot/#User_002dspecific-configuration-1
  ;; https://github.com/elixir-lsp/elixir-ls?tab=readme-ov-file#elixirls-configuration-settings
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-ts-mode heex-ts-mode) .
                   ,(eglot-alternatives '("language_server.sh" "start_lexical.sh"))))))

(use-package ellama
  :disabled
  :custom
  (ellama-user-nick "Juan")
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

(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         :map mode-specific-map
         ("A" . embark-act)
         ("E" . embark-export)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package exec-path-from-shell
  :if (and (memq window-system '(mac ns)) (display-graphic-p))
  :demand
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  ;; https://www.flycheck.org/en/latest/
  :commands (global-flycheck-mode flycheck-mode)
  :init
  (defun flycheck-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap consult-flymake] . consult-flycheck)
               ([remap flymake-show-buffer-diagnostics] . consult-flycheck)
               ([remap flymake-show-diagnostic] . flycheck-display-error-at-point)
               ([remap flymake-show-project-diagnostics] . nil)
               ([remap flymake-goto-next-error] . flycheck-next-error)
               ([remap flymake-goto-prev-error] . flycheck-previous-error)))
  :custom
  (flycheck-indication-mode 'right-fringe)
  :hook
  (flycheck-mode . flycheck-set-bindings))

(use-package forge
  :after magit
  :commands (forge-dispatch)
  :custom
  (auth-sources '("~/.authinfo")))

(use-package geiser-guile
  :commands (geiser geiser-mode)
  :custom
  (geiser-debug-jump-to-debug t)
  :hook
  (scheme-mode . geiser-mode))

(use-package git-link
  :bind (:map global-leader-map ("j y" . git-link)))

(use-package gleam-ts-mode
  :straight (:host github :repo "gleam-lang/gleam-mode")
  :mode (rx ".gleam" eos)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(gleam-ts-mode "gleam" "lsp"))))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4))

(use-package gruber-darker-theme
  :defer t)

(use-package gruvbox-theme
  :defer t)

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("." . helpful-at-point)))

(use-package janet-mode
  :mode "\\.janet$"
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(janet-mode "janet-lsp"))))

(use-package jinx
  ;; Requires OS dependencies.
  :bind (("M-$" . jinx-correct)
         ([remap flyspell-mode] . jinx-mode)))

(use-package js-mode
  :straight nil
  :mode "\\.js\\'"
  :custom
  (js-indent-level 4))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package lsp-mode
  :disabled
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . lsp-format-buffer)
               ([remap xref-find-references] . lsp-find-references)
               ([remap eldoc] . lsp-describe-thing-at-point)))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-set-bindings))

(use-package magit
  :commands (magit-project-status)
  :bind (:map global-leader-map
              ("j j" . magit-status-here)
              ("j m" . magit-blame-addition)
              ("j f" . magit-file-dispatch))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "j")))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate"))

(use-package magit-todos
  :bind (:map project-prefix-map ("t" . magit-todos-list))
  :config
  (magit-todos-mode 1))

(use-package make-mode
  :ensure nil
  :straight nil
  :init
  (defun make-mode-setup ()
    (setq-local outline-regexp "^[A-Za-z].+:"))
  :hook
  (makefile-bsdmake-mode . make-mode-setup))

(use-package marginalia
  :custom
  (completions-detailed nil)
  :config
  (marginalia-mode 1))

(use-package markdown-mode
  :mode "\\.md\\'")

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
     '("f" . meow-find)
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
     '("n" . meow-search)
     '("N" . meow-search-reverse)
     '("o" . other-window)
     '("O" . meow-block)
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
     '("<" . meow-beginning-of-thing)
     '("." . meow-bounds-of-thing)
     '(">" . meow-end-of-thing)
     '("<backspace>" . meow-backward-delete)
     '("<escape>" . meow-cancel-selection)))
  :config
  (meow-setup)
  (meow-global-mode))

(use-package modus-themes
  :defer t)

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package org
  ;; Useful documentation: https://orgmode.org/worg/org-syntax.html
  :defer t
  :ensure nil
  :straight nil
  :commands (org-todo-list
             org-agenda
             org-capture
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
       (shell . t)
       (scheme . t)
       (python . t)))
    (electric-indent-local-mode -1))
  :hook
  (org-mode . org-mode-setup)
  (org-agenda-mode . hl-line-mode)
  :bind (:map global-leader-map
              ("n SPC" . org-search-view)
              ("n a" . org-agenda)
              ("n ," . org-capture-goto-last-stored)
              ("n /" . org-occur-in-agenda-files)
              ("n n" . org-capture)
              ("n t" . org-todo-list)
              :map org-mode-map
              ("M-n" . org-next-visible-heading)
              ("M-p" . org-previous-visible-heading))
  :custom
  (org-directory "~/.notes")
  (org-agenda-files (list org-directory))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'far)
  (org-agenda-window-setup 'only-window)
  (org-columns-default-format "%TODO %ITEM %ALLTAGS %DEADLINE")
  (org-cycle-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars (display-graphic-p))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link nil)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep deadline-up todo-state-down)
     (tags priority-down category-keep)
     (search category-keep)))
  ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
  (org-todo-keywords
   '((sequence "TODO(t!)" "ACTIVE(a!)" "BLOCKED(b@)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-todo-keyword-faces '(("TODO" . "steel blue")
                            ("ACTIVE" . "light goldenrod")
                            ("REVIEW" . "light goldenrod")
                            ("BLOCKED" . "sienna")
                            ("DONE" . "dark olive green")
                            ("CANCELED" . "dim gray")))
  ;; https://orgmode.org/manual/Capture-templates.html
  (org-capture-templates
   `(("t" "Task" entry (file+headline "tasks.org" "Tasks") "* TODO %?\n"
      :prepend t :empty-lines-after 1)
     ("n" "Note" entry (file+headline "notes.org" "Notes") "* %?\n%i"
      :prepend t :empty-lines-after 1)
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %T %?\n%i"
      :prepend t :tree-type month))))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :commands (pinentry-start)
  :init
  (with-eval-after-load 'magit
    (pinentry-start)))

(use-package popper
  :demand
  :bind (:map global-leader-map
              ("o o" . popper-toggle)
              ("o O" . popper-toggle-type))
  :init
  (defun popper-setup ()
    (bind-keys :map (current-local-map)
               ("Q" . popper-kill-latest-popup)
               ("M-n" . popper-cycle)
               ("M-p" . popper-cycle-backwards)))
  (setq popper-reference-buffers
        '(("Output\\*$" . hide)
          occur-mode
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "errors\\*$"
          "\\*Async Shell Command\\*"
          special-mode
          help-mode
          flymake-diagnostics-buffer-mode
          compilation-mode
          comint-mode))
  ;; Match eshell, shell, term, etc
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*.*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode
                  "^\\*vterm.*\\*$"  vterm-mode
                  "^\\*.*eat.*\\*$"  eat-mode)))
  (setq popper-window-height
        (lambda (win)
          (fit-window-to-buffer win (floor (frame-height) 3) 15)))
  :hook
  (popper-open-popup . popper-setup)
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package python-mode
  :ensure nil
  :straight nil
  :mode "\\.py\\'"
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (defun python-setup ()
    (setq-local tab-width 4))
  :hook
  (python-ts-mode . python-setup)
  :custom
  (python-indent-offset 4))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (defun rails-compile ()
    (interactive)
    (setq compile-command
          (cond ((string-match-p "_test.rb\\'" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (relative-file-name)))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list "rails t " file-name))
                     (string-join (list "rails t " (s-concat file-name ":" linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))
  (defun rails-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'rails-compile))
  (defun ruby-setup ()
    (setq-local compile-command "rails t")
    (setq-local outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)")
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . rails-compile)
               ([remap comint-dwim] . rails-comint)))
  :hook
  (ruby-base-mode . ruby-setup)
  :config
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist 'rails-test-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(rails-test-target
                   "^rails test \\([^:]+\\):\\([0-9]+\\)" 1 2 nil nil 1))
    (add-to-list 'compilation-error-regexp-alist 'rspec-backtrace-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(rspec-backtrace-target
                   "^ +# \\(./[A-Za-z0-9][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((ruby-mode ruby-ts-mode)
                   . ("solargraph" "stdio" :initializationOptions
                      (;; options
                       :useBundler t
                       :diagnostics t
                       :completion t
                       :hover t
                       :autoformat :json-false
                       :formatting t
                       :symbols t
                       :definitions t
                       :rename t
                       :references t
                       :folding t))))))

(use-package simple-modeline
  :demand
  :init
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
        (string-truncate-left (relative-file-name) 70)
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
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode
      simple-modeline-segment-spaces)))
  :config
  (simple-modeline-mode))

(use-package sqlformat
  :disabled ;; Requires OS dependency postgresql.
  :commands (sqlformat)
  :init
  (defun sql-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . sqlformat-buffer)))
  :hook
  (sql-mode . sql-set-bindings)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package solarized-theme
  :defer t)

(use-package terraform-mode
  :mode "\\.tf\'"
  :custom
  (terraform-indent-level 2))

(use-package trashed
  :bind (:map global-leader-map
              ("o z" . trashed))
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treemacs
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map global-leader-map
              ("o p" . treemacs-select-window)
              ("o P" . treemacs))
  :custom
  (treemacs-no-png-images t)
  (treemacs-hide-dot-git-directory t)
  :config
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t))

(use-package typescript-ts-mode
  :mode "\\.ts$"
  :custom
  (typescript-ts-mode-indent-offset 4)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode) . ("deno" "lsp")))))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package visual-replace
  :demand
  :config
  (visual-replace-global-mode 1))

(use-package vterm
  :disabled ;; Eat is a better termianl emulator.
  :if (display-graphic-p)
  :bind (:map global-leader-map
              ("o t" . vterm-project)
              ("o T" . vterm-named))
  :init
  (defun vterm-project ()
    (interactive)
    (let ((default-directory (or (project-directory) default-directory)))
      (vterm-other-window)))
  (defun vterm-named ()
    (interactive)
    (vterm (read-string "Session name: ")))
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000))

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package which-key
  :ensure nil
  :straight nil
  :config
  (which-key-mode))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode))

(use-package xref
  :straight nil
  :defer
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily))
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package yaml-ts-mode
  :mode "\\(\\.yaml\\|.yml\\|\\.yaml\\..+\\)\\'")

(use-package yasnippet
  :demand
  ;; https://joaotavora.github.io/yasnippet/index.html
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :disabled ;; Better to rely on custom built templates over externals.
  :after yasnippet)

(use-package zenburn-theme
  :defer t)

(use-package zig-mode
  :mode "\\.zig\\'")
