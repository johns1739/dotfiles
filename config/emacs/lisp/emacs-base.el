;;; emacs-base.el --- Emacs Base Configuration  -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :demand
  :init
  (defvar-keymap global-leader-map :doc "Global leader keymap.")
  (keymap-set global-map "M-SPC" global-leader-map)
  (keymap-set global-leader-map "g" goto-map)
  (keymap-set global-leader-map "s" search-map)
  :bind
  ("C-M-d" . delete-pair)
  ("C-z" . nil)
  ("M-L" . duplicate-dwim)
  ("M-S-SPC" . cycle-spacing)
  ("M-j" . join-line)
  ("M-n" . forward-paragraph)
  ("M-o" . other-window)
  ("M-p" . backward-paragraph)
  ("RET" . newline-and-indent)
  ("s-[" . previous-buffer)
  ("s-]" . next-buffer)
  ([remap backward-sentence] . backward-sexp)
  ([remap downcase-word] . downcase-dwim)
  ([remap forward-sentence] . forward-sexp)
  ([remap split-window-below] . split-window-below-and-jump)
  ([remap split-window-right] . split-window-right-and-jump)
  ([remap upcase-word] . upcase-dwim)
  ( :map global-leader-map
    ("SPC" . project-switch-to-buffer)
    ("." . find-file)
    ("=" . balance-windows-area)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("2" . split-window-below-and-jump)
    ("3" . split-window-right-and-jump)
    ;; Copy/Paste/Edits
    ("x l" . keep-lines)
    ("x k" . delete-matching-lines)
    ("x u" . delete-duplicate-lines)
    ("x s" . sort-lines)
    ("x y" . copy-relative-file-name)
    ("x Y" . copy-absolute-file-name)
    ;; Settings (Look & Feel)
    (", ," . open-custom-file)
    (", ." . open-packages-dired)
    (", +" . global-text-scale-adjust)
    (", =" . balance-windows-area)
    (", D" . toggle-debug-on-error)
    (", F" . toggle-frame-fullscreen)
    (", R" . restart-emacs)
    (", SPC" . load-theme)
    (", c" . display-fill-column-indicator-mode)
    (", f" . toggle-frame-maximized)
    (", h" . hl-line-mode)
    (", r" . reload-emacs)
    (", t" . toggle-truncate-lines)
    (", x" . describe-font)
    :map goto-map
    ("SPC" . switch-to-buffer)
    ("." . xref-find-definitions)
    (">" . eldoc)
    ("," . xref-go-back)
    (";" . scratch-buffer)
    (":" . goto-line)
    ("?" . xref-find-references)
    ("/" . xref-find-apropos)
    ("'" . mode-line-other-buffer)
    ("u" . find-file-at-point)
    ("d" . dired-jump)
    ("i" . imenu)
    ("j" . jump-to-register)
    ("J" . point-to-register)
    ("l" . goto-line)
    ("m" . bookmark-jump)
    ("M" . bookmark-set)
    ;; Window navigation
    ("w h" . windmove-left)
    ("w j" . windmove-down)
    ("w k" . windmove-up)
    ("w l" . windmove-right)
    ("w H" . windmove-swap-states-left)
    ("w J" . windmove-swap-states-down)
    ("w K" . windmove-swap-states-up)
    ("w L" . windmove-swap-states-right)
    :map search-map
    ("j" . list-registers)
    ("m" . list-bookmarks)
    ("o" . occur))
  :custom
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (use-file-dialog nil)
  (visible-bell nil)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  (url-configuration-directory (expand-file-name "cache/url/" user-emacs-directory))
  (truncate-lines t)
  (switch-to-buffer-obey-display-actions t)
  (shr-use-colors nil)
  (set-mark-command-repeat-pop t) ; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
  (ad-redefinition-action 'accept)
  (auto-window-vscroll nil)
  (confirm-kill-emacs 'y-or-n-p)
  (delete-by-moving-to-trash t)
  (delete-pair-blink-delay 0)
  (delete-pair-push-mark t)                   ; EMACS-31 for easy subsequent C-x C-x
  (display-time-default-load-average nil)
  (duplicate-line-final-position 1)
  (fast-but-imprecise-scrolling t)
  (resize-mini-windows 'grow-only)
  (frame-resize-pixelwise t)
  (browse-url-secondary-browser-function 'eww-browse-url) ; C-u C-c RET on URLs open in EWW
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (help-window-select t)
  (history-delete-duplicates t)
  (history-length 1000)
  (ielm-history-file-name (expand-file-name "cache/ielm-history.eld" user-emacs-directory)) ; EMACS-31
  (kill-region-dwim 'emacs-word)
  (imenu-max-item-length 80)
  (native-comp-async-on-battery-power nil)  ; No compilations when on battery EMACS-31
  (multisession-directory (expand-file-name "cache/multisession/" user-emacs-directory))
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (initial-scratch-message nil)
  (kill-do-not-save-duplicates t)
  (max-mini-window-height 0.2)
  (next-error-find-buffer-function 'next-error-buffer-unnavigated-current)
  (next-error-highlight 1.0)
  (next-error-highlight-no-select 1.0)
  (next-error-message-highlight t)
  (next-error-recenter '(4))
  (register-use-preview t)
  (register-preview-delay 0.5)
  (require-final-newline t)
  (read-answer-short t)
  (ring-bell-function 'ignore)
  (scroll-conservatively most-positive-fixnum)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (tab-always-indent t)
  (use-dialog-box nil)
  (use-short-answers t)
  (tab-width 4)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (auto-save-list-file-prefix (expand-file-name "cache/auto-saves/sessions/" user-emacs-directory))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "cache/auto-saves/" user-emacs-directory) t)))
  (prettify-symbols-alist '(("!=" . ?≠)
                            ;; ("&&" . ?∧)
                            ("->" . ?→)
                            ("->>" . ?↠)
                            ("<-" . ?←)
                            ("<<" . ?«)
                            ("<=" . ?≤)
                            ("<>" . ?◇)
                            ("<|" . ?◁)
                            ;; ("==" . ?≡)
                            ("=>" . ?⇒)
                            (">=" . ?≥)
                            (">>" . ?»)
                            ;; ("not" . ?¬)
                            ("|>" . ?▷)
                            ;; ("||" . ?∨)
                            ))
  :hook
  (special-mode . hl-line-mode)
  :config
  (unless (display-graphic-p) ;; When in terminal ...
    (custom-set-faces
     '(default ((((type tty))))))
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
  (when (display-graphic-p) ;; When in GUI ...
    (add-to-list 'default-frame-alist '(height . 40))
    (add-to-list 'default-frame-alist '(width . 120))
    (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
    (set-display-table-slot standard-display-table 'truncation ?\u2192))
  (blink-cursor-mode -1)
  (make-directory (expand-file-name "cache/auto-saves/" user-emacs-directory) t)
  (modify-coding-system-alist 'file "" 'utf-8)
  (setq-default cursor-type 'bar)
  (setq-default display-fill-column-indicator-column 100)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil) ;; use spaces instead of tabs
  (auto-save-visited-mode -1) ;; auto-format constantly triggers, annoying
  (desktop-save-mode -1) ;; CPU heavy when loading many buffers under LSP
  (electric-indent-mode t)
  (column-number-mode -1)
  (global-so-long-mode t)
  (file-name-shadow-mode t)
  (line-number-mode t)
  (repeat-mode -1) ;; Sometimes gets in the way.
  (window-divider-mode (display-graphic-p))
  (defun open-init-file ()
    (interactive)
    (find-file user-init-file))
  (defun open-packages-dired ()
    (interactive)
    (dired (locate-user-emacs-file "lisp/")))
  (defun open-custom-file ()
    (interactive)
    (if (boundp 'custom-file)
        (find-file custom-file)))
  (defun split-window-below-and-jump ()
    "Split window below and jump to it."
    (interactive)
    (select-window (split-window-below)))
  (defun split-window-right-and-jump ()
    "Split window right and jump to it."
    (interactive)
    (select-window (split-window-right)))
  (defun absolute-file-name ()
    "Absolute path to file."
    (expand-file-name (buffer-file-name)))
  (defun copy-absolute-file-name ()
    "Copy absolute file path of current buffer."
    (interactive)
    (let ((afn (absolute-file-name)))
      (kill-new (absolute-file-name))
      (message "Copied %s" afn)))
  (defun project-directory ()
    "Current project directory."
    (let ((project (project-current)))
      (and project (project-root project))))
  (defun relative-file-name ()
    "Relative from project or cwd directory."
    (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))
  (defun copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (let ((rfn (relative-file-name)))
      (kill-new (relative-file-name))
      (message "Copied %s" rfn)))
  (defun reload-emacs ()
    (interactive)
    (load (locate-user-emacs-file "init.el") :no-error-if-file-is-missing)))

;; TODO: How does this work?
(use-package abbrev
  :ensure nil
  :defer
  :custom
  (save-abbrevs nil)
  :config
  (define-abbrev-table 'global-abbrev-table
    '((",uuid" ""
       (lambda () (insert (org-id-uuid)))))))

;; TODO: Learn how authinfo.gpg works
;; password authentication service
;; To reload authinfo:
;; (auth-source-forget-all-cached)
(use-package auth-source
  :ensure nil
  :defer
  :custom
  (epg-pinentry-mode 'loopback)
  (auth-sources '("~/.authinfo" "~/.authinfo.gpg")))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-remote-files nil)   ;; t makes tramp slow
  (auto-revert-verbose t)
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))

(use-package bash-ts-mode
  :ensure nil
  :mode "\\.\\(sh\\|bash\\)\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(bash "https://github.com/tree-sitter/tree-sitter-bash" "master" "src")))

(use-package bookmark
  :ensure nil
  :defer
  :custom
  (bookmark-file (expand-file-name "cache/bookmarks" user-emacs-directory)))

(use-package calc
  :ensure nil
  :bind
  ( :map global-leader-map
    ("k *" . calc)
    :map calc-mode-map
    ("i" . nil)))

(use-package compile
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  :ensure nil
  :bind (:map global-leader-map
              ("k g" . recompile)
              ("k k" . compile-dwim)
              ("k K" . compile)
              ("k RET" . send-region-to-process))
  :custom
  (ansi-color-for-compilation-mode t)
  (compilation-always-kill t)
  (compilation-context-lines 10)
  (compilation-error-regexp-alist '())
  (compilation-error-regexp-alist-alist '())
  (compilation-max-output-line-length 121)
  (compilation-scroll-output 'first-error)
  (compilation-search-path '(nil)) ;; directories to search for files
  (compilation-skip-threshold 2) ;; skip warnings and info with next-error
  (compilation-window-height 20)
  (compile-command "make ")
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  (compilation-mode . hl-line-mode)
  :init
  (defun compile-dwim ()
    (interactive)
    (if (project-current)
        (call-interactively #'project-compile)
      (call-interactively #'compile)))
  (defun send-region-to-process (arg beg end)
    " Send the current region to a process buffer.
    The first time it's called, will prompt for the buffer to
    send to. Subsequent calls send to the same buffer, unless a
    prefix argument is used (C-u), or the buffer no longer has an
    active process. "
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
  ;; Make dir local variables work in compilation buffers
  (add-hook 'compilation-mode-hook 'hack-dir-local-variables-non-file-buffer)
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  ;; (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target
                 "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]" 1 2 nil nil 1))
  ;; (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target
                 "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))

(use-package completion
  :ensure nil
  :custom
  (completion-eager-update t)
  (completion-show-help nil)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-cycle-threshold 3)
  (completion-ignore-case t)
  ;; (completion-styles '(basic substring partial-completion))
  (completion-styles '(partial-completion flex initials))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height nil)
  (completions-sort 'historical)
  ;; preview mode provides its own mode-map that conflicts with regular completion
  (completion-preview-mode nil)
  (completion-pcm-leading-wildcard t)
  :bind ("C-M-i" . completion-at-point))

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'"))

(use-package css-ts-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :config
  (add-to-list 'treesit-language-source-alist
               '(css "https://github.com/tree-sitter/tree-sitter-css")))

(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . dabbrev-completion))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode -1))

(use-package diff-mode
  :ensure nil
  :defer
  :commands (diff diff-mode)
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-update-on-the-fly t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-font-lock-prettify nil)
  :bind
  ( :map diff-mode-map
    ("M-o" . nil)))

(use-package dired
  :ensure nil
  :defer
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-absolute-location t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory))
  :bind
  ( :map search-map
    ("f" . find-name-dired))
  :hook
  (dired-mode . hl-line-mode))

(use-package display-line-numbers
  :ensure nil
  :defer
  :bind
  ( :map global-leader-map
    (", n" . display-line-numbers-mode))
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width 4)
  (display-line-numbers-widen t)
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :defer
  :custom
  (display-fill-column-indicator-warning nil))

(use-package doc-view
  :ensure nil
  :defer
  :custom
  (doc-view-resolution 200))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "Dockerfile.*\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

(use-package ediff
  :ensure nil
  :custom
  (ediff-show-clashes-only t)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-keep-variants nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :bind ( :map global-leader-map
          ("m d" . ediff-files))
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package elec-pair
  :ensure nil
  :defer
  :config
  (electric-pair-mode -1))

(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :bind
  ( :map emacs-lisp-mode-map
    ("C-c t" . ielm)))

(use-package elixir-ts-mode
  :ensure nil
  :mode "\\.exs?\\'"
  :init
  (defun elixir-ts-mode-setup ()
    (setq-local compilation-error-regexp-alist '(elixir-unit-test-target elixir-error-target elixir-warning-target))
    (cond
     ((string-match-p "router.ex$" (buffer-name))
      (setq outline-regexp " *\\(get\\|delete\\|put\\|post\\|scope\\|pipe_through\\|resources\\) "))
     ((string-match-p "_test.exs$" (buffer-name))
      (setq outline-regexp " *\\(describe \\|test \\|setup \\)"))))
  :hook
  (elixir-ts-mode . elixir-ts-mode-setup)
  :config
  (with-eval-after-load 'compile
    ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-unit-test-target "     \\([^ ]+\\.exs\\):\\([0-9]+\\)" 1 2 nil 1 1))
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-error-target "    error:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 1 1))
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(elixir-warning-target "    warning:.+
[ ]+│[^└]+└─ \\([^:() ]+\\):\\([0-9]+\\):?\\([0-9]+\\)" 1 2 3 2 1))))

(use-package eshell
  :bind
  ( :map global-leader-map
    ("k e" . eshell))
  :custom
  (eshell-history-size 100000)
  (eshell-hist-ignoredups t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eshell\\*" (display-buffer-in-side-window)
                 (window-height . 0.3))))

(use-package eglot
  :ensure nil
  :bind ( :map global-leader-map
          ("L" . eglot)
          ("l TAB" . eglot-format)
          ("l e" . eglot-events-buffer)
          ("l E" . eglot-stderr-buffer)
          ("l l" . eglot-reconnect)
          ("l q" . eglot-shutdown)
          ("l Q" . eglot-shutdown-all)
          ("l r" . eglot-rename)
          ("l h" . eglot-inlay-hints-mode)
          ("l d" . eglot-find-declaration)
          ("l a" . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-prefer-plaintext nil)
  (eglot-code-action-indications '(eldoc-hint))
  (eglot-mode-line-session nil))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-help-at-pt t)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  :config
  (global-eldoc-mode t))

(use-package files ;; backups
  :ensure nil
  :custom
  (create-lockfiles nil)
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(expand-file-name "cache/backup/" user-emacs-directory))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 3)
  (make-backup-files t)
  (version-control t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  :config
  (add-to-list 'save-some-buffers-action-alist
               `("d" ,(lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                 "show diff between the buffer and its file"))
  (defun buffer-backed-up-set-to-time ()
    "Set the buffer-backed-up variable to the current time if t."
    (if (eq buffer-backed-up t)
        (setq buffer-backed-up (current-time))))
  (defun buffer-backed-up-reset-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (buffer-backed-up-set-to-time)
    (if (and buffer-backed-up
             (time-less-p (time-add buffer-backed-up (* 60 60 24)) ;; 24hrs
                          (current-time)))
        (setq buffer-backed-up nil))
    (let ((orig-fun-result (apply orig-fun args)))
      (buffer-backed-up-set-to-time)
      orig-fun-result))
  (advice-add 'backup-buffer :around #'buffer-backed-up-reset-advice))


(use-package flymake
  :ensure nil
  :bind
  ( :map global-leader-map
    ("d D" . flymake-mode)
    ("d d" . flymake-show-buffer-diagnostics)
    ("d p" . flymake-show-project-diagnostics))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)
     (warning "?" compilation-warning)
     (note "i" compilation-info))))

(use-package flyspell
  :ensure nil
  :bind ( :map global-leader-map
          ("m $" . flyspell-mode))
  :custom
  (flyspell-delay 1))

(use-package ffap
  :ensure nil
  :commands (find-file-at-point)
  :init
  (defun ffap-deep-match-file (filename)
    (let ((project-dir (project-directory)))
      (or (and project-dir (ffap-deep-match-file-string filename project-dir))
          (ffap-deep-match-file-string filename default-directory))))
  (defun ffap-deep-match-file-string (filename dir)
    (let* ((deep-1 (file-name-concat "**" filename))
           (deep-2 (file-name-concat "**" "**" filename))
           (files  (or (file-expand-wildcards (expand-file-name deep-1 dir) t)
                       (file-expand-wildcards (expand-file-name deep-2 dir) t))))
      (and files (car files))))
  :config
  (add-to-list 'ffap-alist '("" . ffap-deep-match-file)))

(use-package frame
  :ensure nil
  :defer
  :bind
  ("C-x 5 SPC" . select-frame-by-name)
  ("C-x 5 R" . set-frame-name)
  :config
  ;; Make C-x 5 o repeatable
  (defvar-keymap frame-repeat-map
    :repeat t
    "o" #'other-frame
    "n" #'make-frame
    "d" #'delete-frame)
  (put 'other-frame 'repeat-map 'frame-repeat-map))

(use-package go-ts-mode
  ;; Install LSP:
  ;; go install golang.org/x/tools/gopls@latest
  :ensure nil
  :mode "\\.go\\'"
  :mode ("go\\.mod\\'" . go-mod-ts-mode)
  :config
  (add-to-list 'treesit-language-source-alist
               '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (add-to-list 'treesit-language-source-alist
               '(gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(use-package goto-addr
  :ensure nil
  :bind
  ( :map mode-specific-map
    ("C-o" . goto-address-at-point))
  :config
  (global-goto-address-mode -1))

(use-package grep
  :ensure nil
  :bind ( :map search-map
          ("g" . grep)
          ("G" . rgrep))
  :custom
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  :config
  (when (executable-find "rg")
    (setopt grep-command "rg -nS --no-heading ")))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(heex "https://github.com/phoenixframework/tree-sitter-heex")))

(use-package help
  :ensure nil
  :defer
  :bind
  ( :map help-map
    ("M" . describe-keymap)
    ("h" . nil)) ;; accidentally pressed too often
  :custom
  (help-window-keep-selected t)
  (help-window-select 'other))

(use-package hippie-exp
  :ensure nil
  :bind
  ( :map global-map
    ("M-i" . hippie-expand))
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-list
     try-expand-line
     try-expand-dabbrev
     ;; try-expand-list-all-buffers
     try-expand-line-all-buffers
     try-expand-dabbrev-all-buffers
     ;; try-expand-whole-kill ;; use M-y instead
     ;; try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name))
  :init
  (defun hippie-expand-case-fold-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-fold-advice))

(use-package ibuffer
  :ensure nil
  :bind ( :map global-map
          ("C-x C-b" . ibuffer))
  :custom
  (ibuffer-human-readable-size t)
  (ibuffer-expert t)
  (ibuffer-display-summary t)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-title-face 'font-lock-doc-face)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil))

(use-package icomplete
  :disabled
  :ensure nil
  :custom
  (icomplete-delay-completions-threshold 0)
  (icomplete-compute-delay 0)
  (icomplete-show-matches-on-no-input t)
  (icomplete-scroll t)
  :config
  (icomplete-vertical-mode))

(use-package isearch
  :ensure nil
  :defer
  :custom
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?"))

(use-package ispell
  ;; brew install aspell (preferred)
  ;; brew install ispell
  ;; NOTE: ispell fails to install due to compilation issues
  :ensure nil
  :if (or (executable-find "aspell") (executable-find "ispell"))
  :bind ( :map global-map
          ("M-$" . ispell-word))
  :custom
  (text-mode-ispell-word-completion nil)
  :config
  (if (executable-find "aspell")
      (setq ispell-program-name "aspell")))

(use-package js-ts-mode
  :ensure nil
  :mode "\\.jsx?\\'"
  :init
  (defun js-ts-mode-setup ()
    (setq indent-tabs-mode nil))
  :hook
  (js-ts-mode . js-ts-mode-setup)
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist
               '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist
               '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :hook
  (json-ts-mode . js-ts-mode-setup)
  :config
  (add-to-list 'treesit-language-source-alist
               '(json "https://github.com/tree-sitter/tree-sitter-json")))

(use-package log-edit
  :ensure nil
  :defer
  :custom
  (log-edit-confirm 'changed)
  (log-edit-keep-buffer nil)
  (log-edit-require-final-newline t)
  (log-edit-setup-add-author nil))

(use-package make-mode
  :init
  (defun make-mode-setup ()
    (setq-local outline-regexp "^[A-Za-z].+:"))
  :hook
  (makefile-bsdmake-mode . make-mode-setup)
  :config
  (add-to-list 'treesit-language-source-alist
               '(make "https://github.com/alemuller/tree-sitter-make")))

(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :mode ("README\\.md\\'" . gfm-mode) ;; depends on builtin markdown-mode
  :bind ( :map markdown-mode-map
          ("M-H" . markdown-mark-block)
          ("M-n" . markdown-outline-next)
          ("M-p" . markdown-outline-previous)
          ("C-c C-." . markdown-do))
  :custom
  (markdown-command "multimarkdown")
  :config
  (add-to-list 'treesit-language-source-alist
               '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist
               '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(use-package minibuffer
  :ensure nil
  :defer
  :custom
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package org
  :ensure nil
  :init
  (defun org-mode-setup ()
    (electric-indent-local-mode -1))
  :hook
  (org-mode . org-mode-setup)
  (org-agenda-mode . hl-line-mode)
  :bind ( :map global-leader-map
          ("n SPC" . org-search-view)
          ("n a" . org-agenda)
          ("n f" . org-capture-goto-target)
          ("n k" . org-capture)
          ("n r" . org-occur-link-in-agenda-files)
          ("n s" . org-occur-in-agenda-files)
          ("n t" . org-todo-list)
          ("n '" . org-capture-goto-last-stored)
          ("n ," . org-mark-ring-goto)
          ("n L" . org-store-link)
          :map org-mode-map
          ([remap goto-address-at-point] . org-open-at-point)
          ([remap kill-sentence] . org-cut-subtree)
          ("M-H" . org-babel-mark-block)
          ("M-n" . org-next-visible-heading)
          ("M-p" . org-previous-visible-heading)
          ("C-M-a" . org-up-element)
          ("C-M-e" . org-down-element)
          ("M-N" . org-move-subtree-down)
          ("M-P" . org-move-subtree-up))
  :custom
  (org-agenda-sorting-strategy '((todo urgency-down category-keep deadline-up)))
  (org-agenda-tags-column -80)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'far)
  (org-agenda-window-setup 'reorganize-frame)
  (org-archive-location ".archive::* From %s")
  (org-confirm-babel-evaluate nil)
  (org-directory "~/.notes")
  (org-edit-src-content-indentation 0)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1)))
  (org-special-ctrl-a/e t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-tags-column -80)
  ;; https://orgmode.org/manual/Capture-templates.html
  (org-capture-templates
   `(("t" "Task" entry (file+headline "tasks.org" "Task") "* TODO %?\n%i")
     ("n" "Note" entry (file+headline "notes.org" "Note") "* %?\n%i")
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %?\n%T\n%i")
     ("J" "Journal" entry (file+olp+datetree "journal.org") "* JOURNAL\n%T\n%a\n%i"
      :immediate-finish t)))
  :config
  (custom-set-faces
   '(org-todo ((t (:weight bold :foreground "light goldenrod"))))
   '(org-done ((t (:weight bold :foreground "dim gray")))))
  (require 'org-capture)
  (require 'org-crypt)
  (unless (file-exists-p "~/.notes")
    (make-directory "~/.notes"))
  (setopt org-agenda-files (list org-directory))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)))) ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html

(use-package paren
  :custom
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode))

(use-package pixel-scroll
  :ensure nil
  :defer
  :custom
  (pixel-scroll-precision-mode nil)
  (pixel-scroll-precision-use-momentum nil))

(use-package proced
  :ensure nil
  :bind (:map global-leader-map ("k p" . proced))
  :custom
  (proced-tree-flag t)
  (proced-descend t)
  (proced-filter 'user)
  (proced-auto-update-interval 1)
  (proced-auto-update-flag 'visible)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'short))

(use-package project
  :ensure nil
  :defer
  :bind ( :map project-prefix-map
          ("K" . project-forget-project))
  :custom
  (project-list-file (expand-file-name "cache/projects" user-emacs-directory))
  ;; Excellent for mono repos with multiple langs, makes Eglot happy
  ;; (project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod"))
  :init
  (keymap-set global-leader-map "p" project-prefix-map)
  :config
  (require 'vc-git) ;; project-find-file requires vc-git--program-version
  (setopt project-switch-commands '((project-find-regexp "Regexp" ?g)
                                    (project-find-file "File" ?f)
                                    (project-find-dir "Dir" ?d)
                                    (project-eshell "Eshell" ?e)
                                    (project-kill-buffers "Kill" ?k))))

(use-package python
  ;; Example .dir-locals.el to configure compile command.
  ;; ((python-mode . ((eval . (if (and (buffer-file-name)
  ;;                                   (string-match-p "test_.*\\.py" (file-name-nondirectory (buffer-file-name))))
  ;;                              (setq-local compile-command (concat "pytest " (relative-file-name))))))))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :custom
  (python-indent-guess-indent-offset-verbose t)
  (python-indent-offset 4)
  :init
  (defun python-ts-mode-setup ()
    (when (and (buffer-file-name)
               (string-match-p "test_.*\\.py" (file-name-nondirectory (buffer-file-name))))
      (setq-local outline-regexp "\s*\\(def test_\\|class Test\\)")
      (setq-local compile-command (concat "pytest " (relative-file-name)))))
  :hook
  (python-ts-mode . python-ts-mode-setup)
  :config
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist 'python-pytest-target)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(python-pytest-target
                   "^\\([A-Za-z0-9/][^ (]+\\.py\\):\\([1-9][0-9]*\\): "
                   1 2 nil nil 1))))

(use-package recentf
  :ensure nil
  :demand
  :bind
  ( :map goto-map
    ("r" . recentf))
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 300)
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  :config
  (recentf-mode t))

;; TODO: What is this about?
(use-package request
  :defer
  :custom
  (request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(use-package ruby-ts-mode
  :ensure nil
  :mode ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'")
  :interpreter "ruby"
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil)
  :init
  (defun ruby-ts-mode-setup ()
    (setq-local outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)"))
  :hook
  (ruby-ts-mode . ruby-ts-mode-setup)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((ruby-ts-mode)
                   . ("solargraph" "stdio" :initializationOptions
                      ( :useBundler t
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
  (add-to-list 'treesit-language-source-alist
               '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist
               '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))

(use-package savehist
  :ensure nil
  :custom
  (savehist-save-minibuffer-history t)    ; t is default
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
     register-alist                       ; macros
     mark-ring global-mark-ring           ; marks
     search-ring regexp-search-ring))     ; searches
  (savehist-file (expand-file-name "cache/history" user-emacs-directory))
  :config
  (savehist-mode t))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (expand-file-name "cache/saveplace" user-emacs-directory))
  (save-place-limit 600)
  :config
  (save-place-mode t))

(use-package tramp
  :defer
  :custom
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 2)
  :config
  (setq tramp-persistency-file-name (expand-file-name "cache/tramp" user-emacs-directory)))

(use-package transient
  :ensure nil
  :defer
  :custom
  (transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory))
  (transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory)))

(use-package tab-bar
  :if (display-graphic-p)
  :bind
  ( :map goto-map
    ("T" . tab-bar-mode)
    ("t SPC" . tab-bar-switch-to-tab)
    ("t q" . tab-bar-close-tab)
    ("t \"" . tab-bar-switch-to-last-tab)
    ("t t" . tab-bar-new-tab)
    ("t T" . tab-bar-undo-close-tab)
    ("t n" . tab-bar-switch-to-next-tab)
    ("t p" . tab-bar-switch-to-prev-tab)
    ("t N" . tab-bar-move-tab)
    ("t P" . tab-bar-move-tab-backward)
    ("t r" . tab-bar-rename-tab)
    ("t '" . tab-bar-switch-to-recent-tab))
  :init
  (defun tab-bar-tab-name-project ()
    (if (project-current)
        (propertize (format "[%s] " (project-name (project-current))) 'face 'bold)
      (tab-bar-tab-name-current)))
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-show 1)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-auto-width nil)
  (tab-bar-separator "")
  (tab-bar-close-button-show nil)
  (tab-bar-close-button nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-button nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-project)
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))
  :config
  (tab-bar-history-mode 1)
  (custom-set-faces ;; faces must be set in config since not available on init.
   '(tab-bar-tab ((t (:weight bold))))
   '(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground "gray50"))))))

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;; TODO: Figure out what additional auxilary packages work well with treesitter
(use-package treesit
  :ensure nil
  :defer
  :custom
  ;; (treesit--install-language-grammar-out-dir-history (expand-file-name "cache/tree-sitter" user-emacs-directory))
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes t) ;; TODO: Verify major-mode-alist remap variable
  (treesit-font-lock-level 4))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :hook
  (typescript-ts-mode . js-ts-mode-setup)
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'"
  :hook
  (tsx-ts-mode . js-ts-mode-setup)
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist
               '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(use-package uniquify
  :ensure nil
  :defer
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t)
  (uniquify-after-kill-buffer-flag t))

(use-package vc
  :ensure nil
  :defer
  :custom
  (vc-handled-backends '(Git)))

;; TODO: Help windows, re-use windows
(use-package window
  :ensure nil
  :defer
  :bind (("C-x w t"  . window-layout-transpose)
         ("C-x w r"  . window-layout-rotate-clockwise)
         ("C-x w f h"  . window-layout-flip-leftright)
         ("C-x w f v"  . window-layout-flip-topdown)))

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package which-key
  :ensure nil
  :custom
  (which-key-side-window-location 'right)
  (which-key-separator " ")
  (which-key-prefix-prefix "… ")
  (which-key-max-display-columns 3)
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  :config
  (which-key-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . nuke-trailing-whitespace)
  :bind
  ( :map global-leader-map
    ("TAB" . indent-format-buffer)
    :map global-leader-map
    ("x w" . whitespace-cleanup))
  :init
  (defun nuke-trailing-whitespace ()
    ;; Running delete-trailing-whitespace on certain special modes can cause issues.
    ;; So only run in prog-mode.
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace)))
  (defun indent-format-buffer ()
    (interactive)
    (save-excursion
      (whitespace-cleanup)
      (indent-region (point-min) (point-max) nil))))

(use-package xref
  :ensure nil
  :defer
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily))
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (when (executable-find "rg")
    (setopt xref-search-program 'ripgrep)))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")))

(provide 'emacs-base)
;;; emacs-base.el ends here
