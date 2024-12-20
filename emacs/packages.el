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

;; use package settings
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)


;;;; Packages

(use-package ace-window
  :bind  (([remap other-window] . ace-window)))

(use-package avy
  :bind (:map goto-map
              ("l" . avy-goto-line)
              ("g" . avy-goto-char-2)
              ("a g" . avy-goto-char-timer)
              ("a k" . avy-kill-line)
              ("a K" . avy-kill-region)
              ("a m" . avy-move-line)
              ("a M" . avy-move-region)
              ("a y" . avy-copy-line)
              ("a Y" . avy-copy-region)))

(use-package beacon
  :defer 5
  :config
  (beacon-mode 1))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :custom
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-file
         #'cape-dict
         #'cape-elisp-symbol
         ;; #'cape-line ;; Kinda buggy
         )))

;; (use-package common-lisp-mode
;;   :straight nil
;;   :mode
;;   (("\\.lisp$" . common-lisp-mode)
;;    ("\\.clisp$" . common-lisp-mode))
;;   :config
;;   (load (expand-file-name "~/.quicklisp/slime-helper.el") t) ;; t = noerror
;;   (setq inferior-lisp-program "sbcl"))

(use-package consult
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
         ([remap outline-show-only-headings] . consult-outline)
         ([remap project-find-regexp] . consult-ripgrep)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake)
         :map compilation-map
         ("SPC" . consult-compile-error)
         :map search-map
         ("," . consult-ripgrep-symbol-at-point)
         ("L" . consult-focus-lines))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-denote
  :after denote
  :bind (:map notes-map
              ("f" . consult-denote-find)
              ("s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-flycheck
  :after flycheck
  :commands (consult-flycheck))

(use-package corfu
  :defer 5
  :straight (corfu :files (:defaults "extensions/*.el")
                   :includes (corfu-echo corfu-history corfu-popupinfo))
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-auto nil) ; Enable auto completion
  (corfu-auto-delay 0.2) ; Enable auto completion
  (corfu-auto-prefix 2) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-echo-delay 0.3)
  (corfu-preselect 'valid)
  (corfu-separator ?\s)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (global-corfu-mode 1)
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(use-package corfu-terminal
  :defer 5
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package dashboard
  :demand t
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind (([remap rgrep] . deadgrep)))

(use-package denote
  :defer 2
  :bind (:map notes-map
              ("n" . denote)
              ("k" . denote-find-link)
              ("K" . denote-find-backlink)
              ("l" . denote-link-or-create)
              ("L" . denote-link-insert-links-matching-regexp)
              ("r" . denote-rename-file-using-front-matter)
              ("R" . denote-rename-file))
  :custom
  (denote-known-keywords '("task" "doc" "snippet"))
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode))

(use-package diff-hl
  :defer 5
  :if (display-graphic-p)
  :bind (:map git-map
              ("o" . diff-hl-show-hunk)
              ("n" . diff-hl-show-hunk-next)
              ("p" . diff-hl-show-hunk-previous)
              ("S" . diff-hl-stage-dwim)
              ("K" . diff-hl-revert-hunk))
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eat
  :if (display-graphic-p)
  :commands (eat eat-other-window)
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (:map toggle-map
              ("t" . eat-project)
              ("T" . eat)))

(use-package ef-themes
  :if (display-graphic-p))

;; (use-package elixir-ts-mode
;;   :mode (("\\.ex$" . elixir-ts-mode)
;;          ("\\.exs$" . elixir-ts-mode)
;;          ("\\.heex$" . heex-ts-mode))
;;   :init
;;   (defun elixir-setup ()
;;     (setq outline-regexp "\s*\\(describe \\|test \\|setup \\)"))
;;   (with-eval-after-load 'lsp
;;     (setq lsp-elixir-suggest-specs nil))
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs
;;                  `((elixir-ts-mode heex-ts-mode) .
;;                    ,(if (and (fboundp 'w32-shell-dos-semantics)
;;                              (w32-shell-dos-semantics))
;;                         '("language_server.bat")
;;                       (eglot-alternatives
;;                        '("language_server.sh" "start_lexical.sh"))))))
;;   :hook
;;   (elixir-ts-mode . elixir-setup))

;; (use-package elm-mode)

(use-package embark
  :bind (:map compilation-map
              ("a" . embark-act)
              ("A" . embark-act-all)
              ("e" . embark-collect)
              ("E" . embark-export)
              :map help-map
              ("B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (exec-path-from-shell-initialize))

;; (use-package expand-region
;;   :commands (er/expand-region)
;;   :bind ("M-O" . er/expand-region))

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
               ([remap flymake-show-diagnostic] . flycheck-display-error-at-point)
               ([remap flymake-show-buffer-diagnostics] . flycheck-list-errors)
               ([remap flymake-show-project-diagnostics] . nil)
               ([remap flymake-goto-next-error] . flycheck-next-error)
               ([remap flymake-goto-prev-error] . flycheck-previous-error)
               ([remap consult-flymake] . consult-flycheck)))
  :custom
  (flycheck-indication-mode 'right-fringe)
  :hook
  (flycheck-mode . flycheck-set-bindings))

;; (use-package geiser-guile
;;   :commands (geiser-mode))

(use-package git-link
  :bind (:map git-map
              ("y" . git-link)))

;; (use-package gleam-ts-mode
;;   :straight (:host github :repo "gleam-lang/gleam-mode")
;;   :mode (rx ".gleam" eos)
;;   :init
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs
;;                  '(gleam-ts-mode "gleam" "lsp"))))

;; (use-package go-ts-mode
;;   :mode "\\.go\\'")

(use-package gruber-darker-theme
  :if (display-graphic-p))

(use-package helpful
  :bind (:map help-map
              ([remap describe-function] . helpful-callable)
              ([remap describe-command] . helpful-command)
              ([remap describe-variable] . helpful-variable)
              ([remap describe-symbol] . helpful-symbol)
              ([remap describe-key] . helpful-key)
              ("." . helpful-at-point)))

(use-package highlight-indent-guides
  :if (display-graphic-p)
  :bind (:map toggle-map
              ("g" . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-top-character-face-perc 50))

;; (use-package janet-mode
;;   :mode "\\.janet$"
;;   :init
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs
;;                  '(janet-mode "janet-lsp"))))

(use-package jinx
  :defer 5
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; (use-package js
;;   :mode
;;   (("\\.js$" . js-ts-mode)
;;    ("\\.json$" . js-ts-mode))
;;   :init
;;   (defun js-setup ()
;;     (setq outline-regexp " *\\(\".+\"\\) *:"))
;;   :hook
;;   (js-ts-mode . js-setup)
;;   :custom
;;   (js-indent-level 2))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . lsp-format-buffer)))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-set-bindings))

(use-package magit
  :commands (magit-status)
  :bind (:map git-map
              ("SPC" . magit-status)
              ("." . magit-status-here)
              ("f" . magit-file-dispatch)
              ("l" . magit-log-buffer-file)
              ("m" . magit-blame-addition))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "j")))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate"))

(use-package marginalia
  :defer 5
  :init
  (setq completions-detailed nil)
  :config
  (marginalia-mode 1))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package meow
  :demand t
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
    (meow-motion-overwrite-define-key
     '("Q" . meow-quit)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("o" . other-window)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("Q" . "H-Q")
     '("j" . "H-j")
     '("k" . "H-k")
     '("o" . "H-o"))
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
     ;; '("o" . meow-block)
     ;; '("O" . meow-to-block)
     '("o" . other-window)
     '("O" . er/expand-region)
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
  :if (display-graphic-p))

(use-package multiple-cursors
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-N" . mc/unmark-previous-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("M-P" . mc/unmark-next-like-this)
         :map mc/keymap
         ("<return>" . nil)))

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion initials orderless basic)))

(use-package popper
  :defer 2
  :if (display-graphic-p)
  :bind (:map toggle-map
              ("o" . popper-toggle)
              ("O" . popper-toggle-type)
              ("n" . popper-cycle)
              ("p" . popper-cycle-backwards)
              ("q" . popper-kill-latest-popup)
              :repeat-map toggle-cycle-repeat-map
              ("n" . popper-cycle)
              ("p" . popper-cycle-backwards)
              ("q" . popper-kill-latest-popup))
  :init
  (setq popper-reference-buffers
        '(("Output\\*$" . hide)
          (completion-list-mode . hide)
          occur-mode
          "\\*Messages\\*"))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "errors\\*$"
          "\\*Async Shell Command\\*"
          special-mode
          help-mode
          compilation-mode
          comint-mode))
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode
                  "^\\*vterm.*\\*$"  vterm-mode
                  "^\\*eat.*\\*$"  eat-mode
                  )))
  (setq popper-window-height
        (lambda (win)
          (fit-window-to-buffer
           win
           (floor (frame-height) 3)
           12)))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;; (use-package python
;;   :init
;;   (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;;   (defun pytyhon-setup ()
;;     (setq-local tab-width 4))
;;   :hook
;;   (python-ts-mode . pytyhon-setup))

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
                ((string-match-p "engines/flexwork/.+_spec.rb" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (file-relative-name (buffer-file-name)
                                                      (concat (current-directory) "engines/flexwork")))
                       (prefix-command "cd engines/flexwork/ && bundle exec rspec "))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list prefix-command file-name))
                     (string-join (list prefix-command (s-concat file-name ":" linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))
  (defun rails-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'rails-compile))
  (defun ruby-setup ()
    (setq compile-command "rails t")
    (setq outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)")
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . rails-compile)
               ([remap comint] . rails-comint)))
  :hook
  (ruby-base-mode . ruby-setup))

(use-package simple-modeline
  :demand t
  :hook (after-init . simple-modeline-mode)
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
        (string-truncate-left (relative-file-name) 60)
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
      ))))

;; (use-package sqlformat
;;   :commands (sqlformat)
;;   :config
;;   (setq sqlformat-command 'pgformatter)
;;   (setq sqlformat-args '("-s2" "-g")))

;; (use-package typescript-ts-mode
;;   :mode "\\.ts$"
;;   :init
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode) . ("deno" "lsp")))))

(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

;; (use-package vterm
;;   :if (display-graphic-p)
;;   :bind (:map toggle-map
;;               ("t" . vterm-project)
;;               ("T" . vterm-named))
;;   :init
;;   (defun vterm-project ()
;;     (interactive)
;;     (let ((default-directory (or (project-directory) default-directory)))
;;       (vterm-other-window)))
;;   (defun vterm-named ()
;;     (interactive)
;;     (vterm (read-string "Session name: ")))
;;   :custom
;;   (vterm-copy-mode-remove-fake-newlines t)
;;   (vterm-max-scrollback 100000))

(use-package which-key
  :defer 5
  :config
  (which-key-mode))

(use-package xclip
  :defer 2
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

(use-package yaml-ts-mode
  :mode "\\(\\.yaml\\|.yml\\|\\.yaml\\..+\\)\\'")

(use-package yasnippet
  :defer 5
  ;; https://joaotavora.github.io/yasnippet/index.html
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)
