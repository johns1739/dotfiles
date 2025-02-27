;;-*- lexical-binding: t; -*-

;; TODO: Follow prots config suggestions: https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
;; TODO: Need to correctly sort org tasks in agenda.

(defvar-keymap global-leader-map
  :doc "Global leader keymap.")
(keymap-global-set "M-SPC" global-leader-map)

(defvar-keymap notes-map
  :doc "Notes map")
(keymap-set global-leader-map "n" notes-map)

(defvar-keymap diagnostics-map
  :doc "Diagnostics map")
(keymap-set global-leader-map "d" diagnostics-map)

(defvar-keymap compilation-map
  :doc "Compilation map")
(keymap-set global-leader-map "x" compilation-map)

(defvar-keymap open-toggle-map
  :doc "Toggle map")
(keymap-set global-leader-map "o" open-toggle-map)

(defvar-keymap editor-settings-map
  :doc "Editor Settings map")
(keymap-set global-leader-map "e" editor-settings-map)

(defvar-keymap window-map
  :doc "Window movement map")
(keymap-set global-leader-map "w" window-map)
(keymap-set goto-map "w" window-map)

(defvar-keymap tab-movement-map
  :doc "Tab movement map")
(keymap-set global-leader-map "t" tab-movement-map)
(keymap-set goto-map "t" tab-movement-map)

(keymap-set global-leader-map "v" vc-prefix-map)
(keymap-set global-leader-map "c" mode-specific-map)
(keymap-set global-leader-map "g" goto-map)
(keymap-set global-leader-map "s" search-map)
(keymap-set global-leader-map "p" project-prefix-map)

(bind-keys :map global-map
           ("M-i" . completion-at-point)
           ("M-I" . hippie-expand)
           ("M-\\" . cycle-spacing)
           ("C-x C-b" . ibuffer)
           ("M-o" . other-window)
           ("M-#" . dictionary-lookup-definition)
           ("M-L" . duplicate-dwim)
           ("M-j" . comment-indent-new-line)
           ("M-n" . forward-sexp)
           ("M-p" . backward-sexp)

           :map global-leader-map
           ("SPC" . switch-to-buffer)
           ("TAB" . indent-buffer)
           ("=" . balance-windows)
           ("0" . delete-window)
           ("1" . delete-other-windows)
           ("2" . split-window-below-and-jump)
           ("3" . split-window-right-and-jump)

           :map goto-map
           ("SPC" . switch-to-buffer)
           ("." . xref-find-definitions)
           ("," . xref-go-back)
           (";" . scratch-buffer)
           ("?" . xref-find-references)
           ("/" . xref-find-apropos)
           ("'" . mode-line-other-buffer)
           ("%" . xref-find-references-and-replace)
           ("f" . find-file-at-point)
           ("h" . eldoc)
           ("j" . jump-to-register)
           ("J" . point-to-register)
           ("l" . goto-line)
           ("m" . bookmark-jump)
           ("M" . bookmark-set)
           ("n" . next-error)
           ("p" . previous-error)
           ("u" . goto-address-at-point)

           :map search-map
           ("SPC" . project-switch-to-buffer)
           ("." . isearch-forward-thing-at-point)
           ("," . rgrep)
           ("/" . isearch-forward)
           ("d" . project-find-dir)
           ("f" . project-find-file)
           ("i" . imenu)
           ("j" . list-registers)
           ("l" . occur)
           ("o" . outline-show-only-headings)
           ("O" . outline-show-all)
           ("k" . keep-lines)
           ("K" . delete-matching-lines)
           ("s" . project-find-regexp)
           ("r" . recentf-open)
           ("w" . isearch-forward-word)
           ("W" . isearch-forward-symbol)

           :map compilation-map
           ("." . compile-dwim)
           (">" . comint)
           ("," . recompile)
           ("B" . eval-buffer)
           ("g" . recompile)

           :map open-toggle-map
           ("t" . project-eshell)
           ("T" . eshell)

           :map editor-settings-map
           (";" . load-theme)
           ("$" . flyspell-prog-mode)
           ("=" . set-font-size)
           ("," . which-function-mode)
           ("f" . display-fill-column-indicator-mode)
           ("F" . global-display-fill-column-indicator-mode)
           ("h" . hl-line-mode)
           ("H" . global-hl-line-mode)
           ("n" . display-line-numbers-mode)
           ("N" . global-display-line-numbers-mode)
           ("t" . toggle-truncate-lines)
           ("v" . visual-line-mode)
           ("V" . global-visual-line-mode)

           :map diagnostics-map
           (";" . flymake-show-buffer-diagnostics)
           (":" . flymake-show-project-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error)

           :map window-map
           ("SPC" . switch-to-buffer-other-window)
           ("=" . balance-windows)
           ("0" . delete-window)
           ("1" . delete-other-windows)
           ("2" . split-window-below-and-jump)
           ("3" . split-window-right-and-jump)
           ("h" . windmove-left)
           ("j" . windmove-down)
           ("k" . windmove-up)
           ("l" . windmove-right)

           :map tab-movement-map
           ("SPC" . switch-to-buffer-other-tab)
           ("0" . tab-close)
           ("1" . tab-close-other)
           ("2" . tab-new)
           ("," . tab-previous)
           ("." . tab-next)
           (";" . tab-list)
           ("/" . tab-switch)
           ("q" . tab-close)
           ("t" . tab-new)
           ("T" . toggle-frame-tab-bar)
           ("o" . tab-recent)
           ("p" . tab-previous)
           ("n" . tab-next)
           ("u" . tab-undo)

           :map project-prefix-map
           ("SPC" . project-switch-to-buffer)
           ("%" . project-query-replace-regexp)

           :map help-map
           ("h" . nil))

;; isearch settings
(setq isearch-wrap-pause 'no)

;; register settings
(setq register-preview-delay 0.5)

;; duplicating line settings
(setq duplicate-line-final-position 1)

;; navigating errors settings
(setq next-error-recenter '(4))
(setq next-error-highlight 1.0)
(setq next-error-highlight-no-select 1.0)
(setq next-error-message-highlight t)
(setq next-error-find-buffer-function 'next-error-buffer-unnavigated-current)

;; completion settings
(setq completion-at-point-functions '(dabbrev-capf)) ;; available on v29
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (basic partial-completion)))))
(setq completion-cycle-threshold 3)
(setq completion-styles '(substring partial-completion initials flex))
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-max-height 20)
(unless (version< emacs-version "30.0")
  (global-completion-preview-mode -1))

;; tab settings
(setq tab-always-indent t)
(setq-default tab-width 4)

;; xref settings
(setq xref-search-program 'ripgrep)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; hippie settings
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list
      '(try-expand-list
        try-expand-line
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-list-all-buffers
        try-expand-line-all-buffers
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name))
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)

;; diagnostics
(setq flymake-fringe-indicator-position 'right-fringe)

;; process settings
(setq proced-auto-update-interval 1)
(setq proced-enable-color-flag t)
(setq-default proced-auto-update-flag t)
(setq read-process-output-max (* 1024 1024))

;; compilation settings
(setq compilation-window-height 20)
(setq compilation-context-lines 10)
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(setq compilation-error-regexp-alist '())
(setq compilation-error-regexp-alist-alist '())
(with-eval-after-load 'compile
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target
                 "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]" 1 2 nil nil 1))
  (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rails-test-target
                 "^rails test \\([^:]+\\):\\([0-9]+\\)" 1 2 nil nil 1))
  (add-to-list 'compilation-error-regexp-alist 'rails-test-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target
                 "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1))
  (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rspec-backtrace-target
                 "^ +# \\(./[A-Za-z0-9][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1))
  (add-to-list 'compilation-error-regexp-alist 'rspec-backtrace-target)
  (add-hook 'compilation-filter-hook  #'ansi-color-compilation-filter))

;; text / column settings
(column-number-mode -1)
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 100)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; delete settings
(delete-selection-mode -1)

;; desktop settings
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP

;; electric / parens settings
(electric-indent-mode t)
(electric-pair-mode -1)
(setq show-paren-context-when-offscreen 'show-paren-context-when-offscreen)

;; eldoc settings
(global-eldoc-mode t)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;; help / info settings
(add-hook 'special-mode-hook #'hl-line-mode)

;; line settings
(line-number-mode t)
(global-so-long-mode t)
(setq-default display-line-numbers-type t)

;; repeat settings
(repeat-mode -1) ;; Sometimes gets in the way.

;; save settings
(auto-save-visited-mode -1) ;; auto-format constantly triggers, annoying
(save-place-mode t)
(savehist-mode t)

;; space settings
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default indent-tabs-mode nil) ;; use spaces instead of tabs
(setq require-final-newline t)

;; window settings
(window-divider-mode -1)
(setq auto-window-vscroll nil)

;; minibuffer settings
(setq max-mini-window-height 0.2)
(setq enable-recursive-minibuffers t) ;; Might be confusing

;; revert settings
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-avoid-polling t)

;; buffer settings
(setq ibuffer-old-time 24)
(add-to-list 'display-buffer-alist
             '("\\*Help\\*" (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Dictionary\\*" (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*" (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t) (window-height . 10)))

;; history settings
(setq history-delete-duplicates t)
(setq history-length 1000)

;; kill settings
(setq kill-do-not-save-duplicates t)

;; dictionary settings
(setq dictionary-server "dict.org")


;; imenu
(setq imenu-max-item-length 80)

;; eshell settings
(setq eshell-scroll-to-bottom-on-output 'this)

;; recentf settings
(setq recentf-auto-cleanup 300)
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;; password authentication service
;; To reload authinfo:
;; (auth-source-forget-all-cached)
(setq auth-sources '("~/.authinfo"))

;; backup settings
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
(let ((backup-dir (locate-user-emacs-file "backups")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir))
  (setq backup-directory-alist `(("." . ,backup-dir))))

;; scroll settings
(setq scroll-margin 2)
(setq scroll-conservatively most-positive-fixnum)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)
(pixel-scroll-precision-mode t)

;; dired settings
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

;; mouse settings
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; frame settings
(setq-default frame-title-format '("%f"))
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 125)))

;; emacs startup settings
(setopt inhibit-startup-message t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil)
(setq ring-bell-function 'ignore)

;; dialog settings
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq use-dialog-box nil)

;; cursor settings
(setq-default cursor-type 'bar)

;; ffap settings - find-file-at-point
(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1)))

;; treesit settings
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (gleam "https://github.com/gleam-lang/tree-sitter-gleam/")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (scheme "https://github.com/6cdh/tree-sitter-scheme"))))


;; commands & functions

(defun ffap-project-match-1 (name)
  (let ((filename (match-string 1 name)))
    (if (and (project-current) (not (string-prefix-p "./" filename)))
        (expand-file-name filename (project-directory))
      (expand-file-name filename default-directory))))

(defun compile-dwim ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

(defun set-font-size ()
  "Set the font size of Emacs"
  (interactive)
  (let ((font-size (min (max (read-number "Font size: " 12) 10) 24)))
    (set-face-attribute 'default nil :height (* 10 font-size))
    (message "Font size set to %s" font-size)))

(defun split-window-below-and-jump ()
  "Split window below and jump to it."
  (interactive)
  (select-window (split-window-below)))

(defun split-window-right-and-jump ()
  "Split window right and jump to it."
  (interactive)
  (select-window (split-window-right)))

(defun project-directory ()
  "Current project directory."
  (let ((project (project-current)))
    (if project
        (project-root project))))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (indent-region (point-min) (point-max) nil)))

(defun comint ()
  (interactive)
  (universal-argument)
  (command-execute #'compile-dwim))

(defun current-directory ()
  "Current project directory or cwd."
  (or (project-directory) default-directory))

(defun current-directory-base ()
  (f-base (current-directory)))

(defun relative-file-name ()
  "Relative from project or cwd directory."
  (file-relative-name (buffer-file-name) (current-directory)))

(defun absolute-file-name ()
  "Absolute path to file."
  (expand-file-name (buffer-file-name)))

(defun copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (relative-file-name)))

(defun copy-absolute-file-name ()
  "Copy absolute file path of current buffer."
  (interactive)
  (kill-new (absolute-file-name)))

(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)

(defun treesit-install-default-languages ()
  "Install all language grammars registered with Treesitter"
  (interactive)
  (require 'treesit)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; Transparency for terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Load packages
(load (locate-user-emacs-file "packages.el"))

;; custom settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
