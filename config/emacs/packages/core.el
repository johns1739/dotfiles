;;; -*- lexical-binding: t -*-

(use-package ace-window
  :bind  (([remap other-window] . ace-window)
          :map goto-map
          ("w 0" . ace-delete-window)
          ("w 1" . ace-delete-other-windows)
          ("w o" . ace-select-window)
          ("w O" . ace-swap-window)))

(use-package avy
  :bind (([remap goto-line] . avy-goto-line)
         ("C-'" . avy-resume)
         :map global-leader-map
         ("n w" . avy-org-refile-as-child)
         :map isearch-mode-map
         ("C-'" . avy-isearch)
         :map goto-map
         ("g" . avy-goto-word-1)))

(use-package beacon
  :if (display-graphic-p) ;; Not pretty in terminal
  :config
  (beacon-mode 1))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :commands (cape-dict cape-elisp-symbol cape-file cape-history cape-dabbrev cape-line cape-keyword)
  :bind (:map global-leader-map
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
  (compile-command nil)
  (compilation-window-height 20)
  (compilation-context-lines 10)
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (compilation-max-output-line-length 200)
  (compilation-error-regexp-alist '())
  (compilation-error-regexp-alist-alist '())
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
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target
                 "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]" 1 2 nil nil 1))
  (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target
                 "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))


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
         ([remap project-find-regexp] . consult-ripgrep)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake) ;; Errors aren't verbose sometimes
         :map global-leader-map
         ("k SPC" . consult-compile-error)
         :map minibuffer-mode-map
         ("M-i" . consult-history)
         :map search-map
         ("l" . consult-line)
         ("L" . consult-focus-lines)
         :map goto-map
         ("I" . consult-imenu-multi)
         ("o" . consult-outline))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer" "SPC"))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Search" "s")))

(use-package copilot
  ;; Setup
  ;; M-x copilot-install-server
  ;; M-x copilot-login
  :if (executable-find "npm")
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (:map copilot-completion-map
              ("M-f" . copilot-accept-completion-by-word)
              ("M-e" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("M-<tab>" . copilot-accept-completion)
              ("C-M-i" . copilot-accept-completion))
  :custom
  (corfu-auto nil)
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay 0.5)
  :custom-face
  (copilot-overlay-face ((t (:family "JetBrainsMonoNL Nerd Font Mono"
                                     :slant italic
                                     :weight ultra-light
                                     :inherit completions-annotations))))
  :hook
  (prog-mode . copilot-mode))

(use-package corfu
  :demand
  :if (display-graphic-p)
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo corfu-history corfu-popupinfo))
  ;; When corfu-auto is off, better to not modify bindings.
  ;; :bind (:map corfu-map
  ;;             ("TAB" . nil)
  ;;             ("RET" . nil)
  ;;             ("SPC" . nil))
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-separator ?\s)
  (corfu-echo-delay 0.3)
  (corfu-popupinfo-delay '(1.25 . 0.2))
  (corfu-min-width 20)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package deadgrep
  :bind (:map search-map ("g" . deadgrep))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(deadgrep "Deadgrep" "g"))))

(use-package devdocs
  :bind (:map global-leader-map
              ("x h I" . devdocs-install)
              ("x h h" . devdocs-lookup)
              ("x hunquote s" . devdocs-search)))

(use-package dimmer
  :if (display-graphic-p) ;; Only works in GUI
  :config
  (dimmer-mode))

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

(use-package docker
  :if (executable-find "docker")
  :bind (:map global-leader-map
              ("d d" . docker)))

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
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (:map global-leader-map
              ("k t" . eat-project)
              ("k T" . eat))
  :custom
  (eat-term-scrollback-size nil)
  (read-process-output-max (* 32 1024 1024)) ;; 32MB
  :hook
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-load . eat-eshell-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eat\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-min-height . 25))))


(use-package eglot-booster
  ;; cargo install emacs-lsp-booster
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

(use-package exec-path-from-shell
  :if (and (memq window-system '(mac ns x)) (display-graphic-p))
  :demand
  :custom
  (exec-path-from-shell-debug t)
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (exec-path-from-shell-initialize))

(use-package git-link
  :bind (:map global-leader-map
              ("x j" . git-link)
              ("x J" . git-link-dispatch)))

(use-package google-this
  :bind (:map global-leader-map ("o g" . google-this)))

(use-package gptel ;; llm copilot chat
  ;; Copilot settings:
  ;; (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :straight (:nonrecursive t)
  :bind (("C-c RET" . gptel-send)))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("." . helpful-at-point)
         ("F" . helpful-function)))

(use-package indent-bars
  :bind (:map global-leader-map
              ("; g" . indent-bars-mode)))

(use-package kubernetes
  :if (executable-find "kubectl")
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

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
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (completions-detailed nil))

(use-package meow
  :hook
  ((after-init . meow-setup)
   (after-init . meow-global-mode))
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
     '("(" . kmacro-start-macro)
     '(")" . meow-end-or-call-kmacro)
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
     '("<escape>" . meow-cancel-selection))))

(use-package ob-http
  :after org)

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :commands (pinentry-start)
  :init
  (with-eval-after-load 'magit
    (pinentry-start)))

(use-package show-font
  :if (display-graphic-p) ;; none exist in terminal
  :bind ((:map global-leader-map)
         ("; '" . show-font-tabulated)))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
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
      simple-modeline-segment-spaces))))

(use-package sqlformat
  :if (executable-find "pg_format")
  :commands (sqlformat)
  :init
  (defun sql-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-format-buffer] . sqlformat-buffer)))
  :hook
  (sql-mode . sql-set-bindings)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package trashed
  :bind (:map global-leader-map
              ("o z" . trashed))
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package visual-replace
  :hook
  (after-init . visual-replace-global-mode))

(use-package xclip
  :unless (display-graphic-p)
  :hook (after-init . xclip-mode))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :hook (after-init . yas-global-mode)
  :bind (:map goto-map
              ("&" . yas-visit-snippet-file)
              :map global-leader-map
              ("i &" . yas-insert-snippet)
              ("x &" . yas-new-snippet))
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets"))))
