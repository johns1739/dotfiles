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

;;;; Packages

(use-package benchmark-init
  :disabled
  :demand
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package ace-window
  :commands (ace-window)
  :bind  ([remap other-window] . ace-window))

(use-package avy
  :bind (:map goto-map
              ("g" . avy-goto-char-2)
              ("a k" . avy-kill-whole-line)
              ("a K" . avy-kill-region)
              ("a l" . avy-goto-line)
              ("a m" . avy-move-line)
              ("a M" . avy-move-region)
              ("a y" . avy-copy-line)
              ("a Y" . avy-copy-region)))

(use-package beacon
  :defer 3
  :if (display-graphic-p)
  :config
  (beacon-mode 1))

(use-package cc-mode
  :mode "\\.c\\'"
  :ensure nil
  :straight nil
  :defer
  :init
  (add-to-list 'major-mode-remap-alist '(cc-mode . c-ts-mode)))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :commands (cape-dabbrev
             cape--abbrev
             cape-keyword
             cape-file
             cape-dict
             cape-elisp-symbol
             cape-line)
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
  :defer
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  (defun consult-ripgrep-symbol-at-point ()
    (interactive)
    (consult-ripgrep nil (format "%s -- -w" (thing-at-point 'symbol))))
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
         ([remap occur] . consult-line)
         ([remap project-find-regexp] . consult-ripgrep)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake)
         :map compilation-map
         ("SPC" . consult-compile-error)
         :map search-map
         (">" . consult-ripgrep-symbol-at-point)
         ("L" . consult-focus-lines)
         ("o" . consult-outline))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-denote
  :bind (:map notes-map
              ("f" . consult-denote-find)
              ("s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-flycheck
  :commands (consult-flycheck))

(use-package copilot
  :disabled ;; requires copilot subscription token
  :defer 3
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
  :disabled ;; intrusive, ruins flow.
  :demand
  :if (display-graphic-p)
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo corfu-history corfu-popupinfo))
  :bind (:map corfu-map ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-echo-delay 0.3)
  (corfu-preselect 'valid)
  (corfu-separator ?\s)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-min-width 20)
  (corfu-preview-current nil)
  :config
  (global-corfu-mode 1)
  (if (display-graphic-p)
      (corfu-popupinfo-mode 1)
    (corfu-echo-mode 1))
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :disabled ;; because corfu is disabled
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package csv-mode
  :mode "\\.csv\\'")

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
  :bind (:map search-map (";" . deadgrep)))

(use-package denote
  :bind (:map notes-map
              ("SPC" . denote-open-or-create)
              ("c" . denote)
              ("j" . denote-journal-extras-new-or-existing-entry)
              ("l" . denote-link-or-create)
              ("k" . denote-finlink)
              ("K" . denote-finbacklink)
              ("r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory notes-directory)
  (denote-known-keywords '("private"))
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode))

(use-package diff-hl
  :defer 3
  :bind (:map vc-prefix-map
              ("," . diff-hl-show-hunk))
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
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(eat-project "Terminal" "t"))))

(use-package ef-themes
  :defer t)

(use-package eldoc-box
  :disabled ;; Annoying
  :if (display-graphic-p)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

(use-package elixir-ts-mode
  :mode (("\\.ex$" . elixir-ts-mode)
         ("\\.exs$" . elixir-ts-mode)
         ("\\.heex$" . heex-ts-mode))
  :custom
  (lsp-elixir-suggest-specs nil)
  :init
  (defun elixir-setup ()
    (setq outline-regexp "\s*\\(describe \\|test \\|setup \\)"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-ts-mode heex-ts-mode) .
                   ,(if (and (fboundp 'w32-shell-dos-semantics)
                             (w32-shell-dos-semantics))
                        '("language_server.bat")
                      (eglot-alternatives
                       '("language_server.sh" "start_lexical.sh"))))))
  :hook
  (elixir-ts-mode . elixir-setup))

(use-package ellama
  :defer 3
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
  :demand
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :disabled ;; rarely used
  :commands (er/expand-region)
  :bind ("M-O" . er/expand-region))

(use-package flycheck
  ;; https://www.flycheck.org/en/latest/
  :bind (:repeat-map flycheck-error-repeat-map
                     ("n" . flycheck-next-error)
                     ("p" . flycheck-previous-error)
                     ("." . flycheck-display-error-at-point))
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
  :disabled ;; requires host authentication token
  :defer 3
  :after magit)

(use-package geiser-guile
  :commands (geiser geiser-mode)
  :custom
  (geiser-debug-jump-to-debug t)
  :hook
  (scheme-mode . geiser-mode))

(use-package git-link
  :bind (:map vc-prefix-map ("y" . git-link)))

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

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("." . helpful-at-point)))

(use-package highlight-indent-guides
  :disabled ;; interferes with treesitter font-locking
  :if (display-graphic-p)
  :bind (:map editor-settings-map
              ("g" . highlight-indent-guides-mode))
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-top-character-face-perc 25))

(use-package indent-bars
  ;; Can replace highlight-indent-guides
  :disabled) ;; Works only on mac Carbon version, not NS version: (version)

(use-package janet-mode
  :mode "\\.janet$"
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(janet-mode "janet-lsp"))))

(use-package jinx
  ;; Requires OS dependencies.
  :bind (("M-$" . jinx-correct)
         ([remap flyspell-mode] . jinx-mode)))

(use-package lsp-mode
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
  :commands (magit-status magit-file-dispatch magit-blame-addition)
  :bind (:map vc-prefix-map
              (";" . magit-status)
              (":" . magit-dispatch)
              ("." . magit-file-dispatch)
              ("g" . magit-blame-addition))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" ";")))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate"))

(use-package magit-todos
  :bind (:map open-toggle-map ("l" . magit-todos-list))
  :config
  (magit-todos-mode 1))

(use-package marginalia
  :defer 3
  :init
  (setq completions-detailed nil)
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
  :init
  (defun meow-setup ()
    (setq meow-cursor-type-motion '(hbar . 2))
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
     '("m" . meow-join)
     '("M" . meow-pop-to-mark)
     '("n" . meow-search)
     '("N" . meow-unpop-to-mark)
     '("o" . other-window)
     '("O" . meow-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . nil) ;; Keep q unbound for other apps to bind.
     '("Q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     (cons "s" search-map)
     '("S" . save-buffer)
     '("t" . meow-till)
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
  (meow-global-mode 1))

(use-package modus-themes
  :defer t)

(use-package multiple-cursors
  :disabled ;; rarely used, meow cursor tends to be better
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-N" . mc/unmark-previous-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("M-P" . mc/unmark-next-like-this)
         :map mc/keymap
         ("<return>" . nil)))

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion initials orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package org
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
               ([remap goto-address-at-point] . org-open-at-point))
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
  :bind (:map notes-map
              (";" . org-todo-list)
              (":" . org-agenda-list)
              ("," . org-store-link)
              ("<" . org-insert-link)
              ("." . org-capture)
              (">" . org-capture-goto-last-stored)
              ("/" . org-search-view)
              ("?" . org-occur-in-agenda-files)
              :map org-mode-map
              ("M-n" . org-next-visible-heading)
              ("M-p" . org-previous-visible-heading))
  :custom
  (org-agenda-todo-ignore-deadlines 'far)
  (org-cycle-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link nil)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-columns-default-format "%TODO %ITEM %ALLTAGS %DEADLINE")
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep deadline-up scheduled-up todo-state-down)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-todo-keywords
   '((sequence "TODO(t!)" "ACTIVE(a!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELED(c@/!)")))
  (org-tag-faces '(("bug"  . "sienna")
                   ("feature" . "goldenrod")
                   ("chore" . "khaki")))
  (org-todo-keyword-faces '(("TODO" . "goldenrod")
                            ("ACTIVE" . "dark khaki")
                            ("DONE" . "dark olive green")
                            ("CANCELED" . "sienna")))
  (org-capture-templates
   `(("t" "Task" entry (file+headline "tasks.org" "Tasks") "* %?" :prepend t :empty-lines 1))))

(use-package popper
  :defer 3
  :bind (:map open-toggle-map
              ("o" . popper-toggle)
              ("O" . popper-toggle-type))
  :init
  (defun popper-setup ()
    (bind-keys :map (current-local-map)
               ("Q" . popper-kill-latest-popup)
               ("M-n" . popper-cycle)
               ("M-p" . popper-cycle-backwards)))
  (setq popper-reference-buffers
        '(("Output\\*$" . hide)
          (completion-list-mode . hide)
          occur-mode
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "errors\\*$"
          "\\*Async Shell Command\\*"
          special-mode
          help-mode
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

(use-package project
  :defer t
  :straight nil
  :custom
  (project-switch-commands '((project-find-file "Find file" "f")
                             (project-find-dir "Find directory" "d")
                             (project-vc-dir "VC-Dir" "v")
                             (project-eshell "Eshell" "e"))))

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
  :init
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
                       :folding t)))))
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
  (ruby-base-mode . ruby-setup))

(use-package simple-modeline
  :demand
  :init
  (defun simple-modeline-segment-project-name ()
    "Display project name in mode line."
    (if (project-current)
        (propertize (project-name (project-current)) 'face 'bold)))
  (defun simple-modeline-segment-buffer-name-2 ()
    "Display buffer's relative-name in mode line."
    (propertize (concat "  " (mode-line-buffer-name)) 'face 'mode-line-buffer-id))
  (defun simple-modeline-segment-end-spaces ()
    (propertize "  "))
  (defun mode-line-buffer-name ()
    (if (buffer-file-name)
        (string-truncate-left (relative-file-name) 70)
      (buffer-name)))
  :custom
  (simple-modeline-segments
   '((
      meow-indicator
      simple-modeline-segment-modified
      ;; simple-modeline-segment-project-name
      ;; simple-modeline-segment-buffer-name
      simple-modeline-segment-buffer-name-2
      simple-modeline-segment-position)
     (
      ;; simple-modeline-segment-minor-modes
      ;; simple-modeline-segment-input-method
      ;; simple-modeline-segment-eol
      ;; simple-modeline-segment-encoding
      ;; simple-modeline-segment-vc
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode
      simple-modeline-segment-end-spaces
      )))
  :config
  (simple-modeline-mode 1))

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

(use-package trashed
  :bind (:map open-toggle-map ("z" . trashed))
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treemacs
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map open-toggle-map
              ("P" . treemacs)
              ("p" . treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t))

(use-package typescript-ts-mode
  :mode "\\.ts$"
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode) . ("deno" "lsp")))))

(use-package vertico
  :demand
  :config
  (vertico-mode 1))

(use-package visual-replace
  :defer 3
  :config
  (visual-replace-global-mode 1))

(use-package vterm
  :disabled ;; Eat is a better termianl emulator.
  :if (display-graphic-p)
  :bind (:map open-toggle-map
              ("t" . vterm-project)
              ("T" . vterm-named))
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

(use-package which-key
  :defer 3
  :config
  (which-key-mode))

(use-package xclip
  :demand
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

(use-package yaml-ts-mode
  :mode "\\(\\.yaml\\|.yml\\|\\.yaml\\..+\\)\\'")

(use-package yasnippet
  :defer 3
  ;; https://joaotavora.github.io/yasnippet/index.html
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :disabled ;; Better to rely on custom built templates over externals.
  :after yasnippet)

(use-package zig-mode
  :mode "\\.zig\\'")
