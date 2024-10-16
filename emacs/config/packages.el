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

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package embark
  :bind (:map global-leader-map
          ("a" . embark-act)
          ("A" . embark-act-all)
          ("e" . embark-collect)
          ("E" . embark-export)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package undo-tree
;;   :custom
;;   (undo-tree-visualizer-timestamps t)
;;   :config
;;   (let ((undo-tree-history-directory (locate-user-emacs-file "undo-tree-history")))
;;     (unless (file-exists-p undo-tree-history-directory)
;;       (make-directory undo-tree-history-directory))
;;     (setq undo-tree-history-directory-alist
;;           `(("." . ,undo-tree-history-directory))))
;;   (global-undo-tree-mode 1))

;; (use-package copilot
;;   :disabled t
;;   :if (display-graphic-p)
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :bind (:map copilot-completion-map
;;               ("M-f" . copilot-accept-completion-by-word)
;;               ("M-e" . copilot-accept-completion-by-line)
;;               ("M-n" . copilot-next-completion)
;;               ("M-p" . copilot-previous-completion)
;;               ("M-<tab>" . copilot-accept-completion))
;;   :custom
;;   (copilot-indent-offset-warning-disable t)
;;   :hook
;;   (prog-mode . copilot-mode)
;;   :config
;;   (set-face-attribute 'copilot-overlay-face nil :family "Monaspace Krypton" :slant 'italic))

(use-package which-key
  :config
  (which-key-mode))

(use-package highlight-indent-guides
  :if (display-graphic-p)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-auto-even-face-perc 0)
  (highlight-indent-guides-auto-odd-face-perc 0)
  (highlight-indent-guides-auto-character-face-perc 200))

(use-package ace-window
  :defer t
  :bind  (([remap other-window] . ace-window)
      ;; ([remap delete-window] . ace-delete-window)
      ;; ([remap delete-other-windows] . ace-delete-other-windows)
      ([remap window-swap-states] . ace-swap-window)
      ([remap evil-window-next] . ace-window)))

(use-package avy
  :defer t
  :bind (:map goto-map
          ("l" . avy-goto-line)
          ("g" . avy-goto-char-2)))

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
     :map diagnostics-map
     ("/" . consult-flymake)
     :map compilation-map
     ("/" . consult-compile-error)
     :map goto-map
     ("j" . consult-register-load)
     ("J" . consult-register-store)
     :map search-map
     ("SPC" . consult-project-buffer)
     ("." . consult-ripgrep-symbol-at-point)
     ("o" . consult-outline)
     ("h" . consult-info)
     ("i" . consult-imenu)
     ;; ("I" . consult-imenu-multi) -- takes too long to be useful.
     ("j" . consult-register)
     ("k" . consult-keep-lines)
     ("l" . consult-line)
     ("L" . consult-focus-lines)
     ("m" . consult-mark)
     ("M" . consult-global-mark)
     ("s" . consult-ripgrep)
     ("y" . consult-yank-from-kill-ring))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  :straight (corfu :files (:defaults "extensions/*.el")
           :includes (corfu-echo corfu-history corfu-popupinfo))
  :bind (:map corfu-map
          ("RET" . nil))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-delay 0.5) ; Enable auto completion
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
  :unless (display-graphic-p)
  :requires corfu
  :config
  (corfu-terminal-mode 1))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :bind (:map completion-map
          ("." . cape-dabbrev)
          ("a" . cape-abbrev)
          ("e" . cape-elisp-block)
          ("f" . cape-file)
          ("h" . cape-history)
          ("k" . cape-keyword)
          ("l" . cape-line)
          ("s" . cape-elisp-symbol)
          ("d" . cape-dict))
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

(use-package dumb-jump
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package vertico
  :init
  ;; Ensure builtins are turned off.
  (icomplete-vertical-mode -1)
  (icomplete-mode -1)
  (fido-mode -1)
  (fido-vertical-mode -1)
  (setq completion-cycle-threshold nil)
  :config
  (vertico-mode 1))

;; ;; Buggy, sometimes data gets clipped
;; (use-package vertico-posframe
;;   :if (display-graphic-p)
;;   :config
;;   (vertico-posframe-mode 1))

(use-package marginalia
  :init
  (setq completions-detailed nil)
  :config
  (marginalia-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (exec-path-from-shell-initialize))

;; (use-package flycheck
;;   ;; https://www.flycheck.org/en/latest/
;;   :defer t
;;   :bind (:repeat-map flycheck-error-repeat-map
;;                      ("n" . flycheck-next-error)
;;                      ("p" . flycheck-previous-error)
;;                      ("." . flycheck-display-error-at-point))
;;   :init
;;   (defun flycheck-set-bindings ()
;;     (bind-keys :map (current-local-map)
;;                ([remap consult-flymake] . consult-flycheck)
;;                ([remap flymake-show-diagnostic] . flycheck-display-error-at-point)
;;                ([remap flymake-show-buffer-diagnostics] . flycheck-list-errors)
;;                ([remap flymake-show-project-diagnostics] . nil)
;;                ([remap flymake-goto-next-error] . flycheck-next-error)
;;                ([remap flymake-goto-prev-error] . flycheck-previous-error)))
;;   :custom
;;   (flycheck-indication-mode 'right-fringe)
;;   :hook
;;   (flycheck-mode . flycheck-set-bindings))

;; (use-package consult-flycheck
;;   :defer t
;;   :commands (consult-flycheck))

(use-package magit
  :commands (magit-status)
  :bind (:map git-map
          ("," . magit-status-here)
          (";" . magit-status)
          ("f" . magit-file-dispatch)
          ("l" . magit-log-buffer-file)
          ("m" . magit-blame-addition))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "j")))
  :config
  ;; (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-list-refs-sortby "-creatordate"))

(use-package git-link
  :bind (:map git-map
          ("y" . git-link)))

(use-package diff-hl
  :if (display-graphic-p)
  :bind (:map git-map
          ("." . diff-hl-show-hunk)
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

;; (use-package magit-todos
;;   ;; Too slow for api-app
;;   :after magit
;;   :config
;;   (magit-todos-mode 1))

(use-package dashboard
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package popper
  :demand t
  :if (display-graphic-p)
  :bind (:map toggle-map
          ("o" . popper-toggle)
          ("O" . popper-toggle-type)
          ("n" . popper-cycle)
          ("p" . popper-cycle-backwards)
          ("Q" . popper-kill-latest-popup)
          :repeat-map toggle-cycle-repeat-map
          ("n" . popper-cycle)
          ("p" . popper-cycle-backwards)
          ("Q" . popper-kill-latest-popup))
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
        '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
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

(use-package vterm
  :if (display-graphic-p)
  :bind (:map toggle-map
          ("t" . vterm-project)
          ("T" . vterm))
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
  (vterm-max-scrollback 10000))

;; aka Zen mode
(use-package writeroom-mode
  :if (display-graphic-p)
  :bind (:map toggle-map
          ("z" . writeroom-mode)
          ("Z" . global-writeroom-mode)))

(use-package disable-mouse
  :unless (display-graphic-p)
  :config
  (global-disable-mouse-mode))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package python-ts-mode
  :straight nil ;; python-ts-mode is already built-in
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (defun pytyhon-setup ()
    (setq-local tab-width 4))
  :hook
  (python-ts-mode . pytyhon-setup))

(use-package ruby-ts-mode
  :defer t
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
    (setq compile-command "rails t")
    (setq outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)")
    (bind-keys :map (current-local-map)
           ([remap compile-dwim] . rails-compile)
           ([remap comint] . rails-comint)))
  :hook
  (ruby-base-mode . ruby-setup))

(use-package elixir-ts-mode
  :mode (("\\.ex$" . elixir-ts-mode)
     ("\\.exs$" . elixir-ts-mode)
     ("\\.heex$" . heex-ts-mode))
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

(use-package gleam-ts-mode
  :straight (:host github :repo "gleam-lang/gleam-mode")
  :mode (rx ".gleam" eos)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
         '(gleam-ts-mode "gleam" "lsp"))))

(use-package go-ts-mode
  :mode "\\.go\\'")

(use-package elm-mode
  :defer t)

(use-package js
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2))

(use-package erlang
  :straight nil
  :load-path "/opt/homebrew/lib/erlang/lib/tools-3.6/emacs/"
  :init
  (setq erlang-root-dir "/opt/homebrew/lib/erlang")
  (add-to-list 'exec-path "/opt/homebrew/lib/erlang/bin")
  :mode
  (("\\.erl?$" . erlang-mode)
   ("rebar\\.config$" . erlang-mode)
   ("relx\\.config$" . erlang-mode)
   ("sys\\.config\\.src$" . erlang-mode)
   ("sys\\.config$" . erlang-mode)
   ("\\.config\\.src?$" . erlang-mode)
   ("\\.config\\.script?$" . erlang-mode)
   ("\\.hrl?$" . erlang-mode)
   ("\\.app?$" . erlang-mode)
   ("\\.app.src?$" . erlang-mode)
   ("\\Emakefile" . erlang-mode))
  :config
  (require 'erlang-start))

(use-package yaml-ts-mode
  :defer t
  :mode "\\(\\.yaml\\|.yml\\)\\'")

(use-package sqlformat
  :commands (sqlformat)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package janet-mode
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
         '(janet-mode "janet-lsp"))))

(use-package geiser-guile
  :commands (geiser-mode))

(use-package multiple-cursors
  :bind (("M-n" . mc/mark-next-like-this)
     ("M-p" . mc/mark-previous-like-this)
     :map mc/keymap
     ("<return>" . nil)))

(use-package expand-region
  :commands (er/expand-region)
  :bind ("M-O" . er/expand-region))

(use-package gruber-darker-theme
  :if (display-graphic-p))

;; (use-package gruvbox-theme)

;; (use-package timu-rouge-theme)

;; (use-package catppuccin-theme
;;   :config
;;   ;; (catppuccin-reload)
;;   (setq catppuccin-flavor 'mocha)) ;; 'frappe, 'latte, 'macchiato, or 'mocha

(use-package ef-themes
  :if (display-graphic-p))

;; (use-package solarized-theme)

;; (use-package modus-themes)

;; (use-package doom-modeline
;;   :custom
;;   (doom-modeline-icon nil)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-vcs-max-length 20)
;;   (doom-modeline-display-misc-in-all-mode-lines nil)
;;   (doom-modeline-env-version nil)
;;   :config
;;   (doom-modeline-mode 1))

(use-package meow
  :custom
  (meow-use-clipboard t)
  (meow-visit-collect-min-length 1)
  (meow-keypad--self-insert-undefined nil)
  (meow-expand-hint-remove-delay 2)
  :init
  (defun meow-setup ()
    (setq mode-line-front-space "")
    (setq meow-cursor-type-motion '(hbar . 2))
    (set-face-attribute 'meow-insert-indicator nil :inherit 'bold)
    (set-face-attribute 'meow-beacon-indicator nil :inherit 'bold-italic)
    (set-face-attribute 'meow-motion-indicator nil :inherit 'italic)
    (add-to-list 'meow-expand-exclude-mode-list 'help-mode)
    (meow-setup-indicator)
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
     '("o" . "H-o")

     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))

    (meow-normal-define-key
     '("=" . meow-repeat)
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

     '("+" . nil)
     '("!" . meow-kmacro-lines)
     '("@" . meow-start-kmacro)
     '("#" . meow-end-or-call-kmacro)
     '("$" . meow-kmacro-matches)
     '("%" . query-replace)
     '("M-%" . meow-query-replace-regexp)
     '("^" . delete-indentation)
     '("&" . async-shell-command)
     '("(" . meow-start-kmacro)
     '(")" . meow-end-or-call-kmacro)
     '("_" . meow-reverse)

     '("a" . meow-append)
     '("A" . meow-open-below)

     '("b" . meow-back-word)
     '("B" . meow-back-symbol)

     '("c" . meow-change)
     (cons "C" compilation-map)

     '("d" . meow-delete)
     '("D" . meow-kill)

     '("e" . meow-next-word)
     '("E" . meow-next-symbol)

     '("f" . meow-find)
     '("F" . nil)

     (cons "g" goto-map)
     '("G" . meow-grab)

     '("h" . meow-left)
     '("H" . mark-paragraph)

     '("i" . meow-insert)
     '("I" . meow-open-above)

     '("j" . meow-next)
     (cons "J" git-map)

     '("k" . meow-prev)
     (cons "K" diagnostics-map)

     '("l" . meow-right)
     '("L" . duplicate-dwim)

     '("m" . meow-join)
     '("M" . nil)

     '("n" . meow-search)
     (cons "N" notes-map)

     '("o" . other-window)
     (cons "O" toggle-map)

     '("p" . meow-yank)
     (cons "P" project-prefix-map)

     '("q" . nil) ;; Keep q unbound for other apps to bind.
     '("Q" . meow-quit)

     '("r" . meow-replace)
     '("R" . meow-swap-grab)

     (cons "s" search-map)
     '("S" . save-buffer)

     '("t" . meow-till)
     '("T" . nil)

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
     '("Z" . meow-sync-grab)

     ;; Keep these unbound for other apps to bind.
     '("<tab>" . nil)
     '("<down>" . nil)
     '("<up>" . nil)
     '("<right>" . nil)
     '("<left>" . nil)

     '("\\" . cycle-spacing)
     '("|" . repeat-complex-command)

     '("'" . meow-last-buffer)
     '("\"" . nil)

     '(";" . meow-comment)
     '(":" . goto-line)

     '("/" . meow-visit)
     '("?" . isearch-forward-thing-at-point)

     '("," . meow-inner-of-thing)
     '("<" . beginning-of-buffer)

     '("." . meow-bounds-of-thing)
     '(">" . end-of-buffer)

     '("[" . meow-beginning-of-thing)
     '("{" . nil)

     '("]" . meow-end-of-thing)
     '("}" . nil)

     '("`" . nil)
     '("~" . nil)

     '("<backtab>" . indent-buffer)
     '("<escape>" . meow-cancel-selection)))

  :config
  (meow-setup)
  (meow-global-mode 1))
