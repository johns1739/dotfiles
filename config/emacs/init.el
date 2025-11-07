;;-*- lexical-binding: t; -*-

(defvar-keymap global-leader-map :doc "Global leader keymap.")
(keymap-set ctl-x-map "SPC" global-leader-map)
(keymap-set global-leader-map "g" goto-map)
(keymap-set global-leader-map "s" search-map)

(bind-keys :map global-map
           ([remap backward-sentence] . backward-sexp)
           ([remap forward-sentence] . forward-sexp)
           ([remap split-window-below] . split-window-below-and-jump)
           ([remap split-window-right] . split-window-right-and-jump)
           ([remap downcase-word] . downcase-dwim)
           ([remap upcase-word] . upcase-dwim)

           ("C-M-;" . comment-indent)
           ("C-j" . comment-indent-new-line)
           ("M-i" . completion-at-point)
           ("M-j" . join-line)
           ("M-L" . duplicate-dwim)
           ("M-n" . forward-paragraph)
           ("M-o" . other-window)
           ("M-p" . backward-paragraph)

           ;; Global Leader Bindings
           :map global-leader-map
           ("TAB" . indent-format-buffer)
           ("SPC" . project-switch-to-buffer)
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
           ("x w" . whitespace-cleanup)

           ;; Package Modes
           ("m <tab>" . toggle-truncate-lines)
           ("m c" . display-fill-column-indicator-mode)
           ("m C" . global-display-fill-column-indicator-mode)
           ("m h" . hl-line-mode)
           ("m H" . global-hl-line-mode)
           ("m n" . display-line-numbers-mode)
           ("m N" . global-display-line-numbers-mode)

           ;; Compilation & Computation
           ("k c" . calc)
           ("k r" . regexp-builder)

           ;; Settings (Look & Feel)
           (", ," . open-custom-file)
           (", SPC" . load-theme)
           (", =" . global-text-scale-adjust)
           (", D" . toggle-debug-on-error)
           (", x" . describe-font)
           (", f" . toggle-frame-maximized)
           (", F" . toggle-frame-fullscreen)
           (", r" . reload-emacs)
           (", R" . restart-emacs)

           :map mode-specific-map
           ("C-o" . goto-address-at-point)

           :map goto-map
           ("SPC" . switch-to-buffer)
           ("." . xref-find-definitions)
           ("," . xref-go-back)
           (";" . scratch-buffer)
           (":" . goto-line)
           ("?" . xref-find-references)
           ("/" . xref-find-apropos)
           ("'" . mode-line-other-buffer)
           ("f" . find-file-at-point)
           ("d" . dired-jump)
           ("h" . eldoc)
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
           ("b" . ibuffer)
           ("g" . rgrep)
           ("j" . list-registers)
           ("m" . list-bookmarks)
           ("o" . occur)
           ("r" . recentf-open))

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
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (basic partial-completion)))))
(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)
(setq completion-styles '(basic substring partial-completion))
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-max-height 20)

;; tab settings
(setq tab-always-indent t)
(setq-default tab-width 4)

;; column settings
(setq-default display-fill-column-indicator-column 100)
(column-number-mode -1)

;; text settings
(setq-default fill-column 80)
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

;; repeat settings
(repeat-mode -1) ;; Sometimes gets in the way.

;; save settings
(auto-save-visited-mode -1) ;; auto-format constantly triggers, annoying
(save-place-mode t)
(savehist-mode t)

;; whitespace settings
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

;; buffer settings & display
(setq ibuffer-old-time 24)

;; history settings
(setq history-delete-duplicates t)
(setq history-length 1000)

;; kill settings
(setq kill-do-not-save-duplicates t)

;; imenu
(setq imenu-max-item-length 80)

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
(let ((backup-directory (locate-user-emacs-file "backup-versions")))
  (unless (file-exists-p backup-directory)
    (make-directory backup-directory))
  (setq backup-directory-alist `(("." . ,backup-directory))))

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
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'compilation-mode-hook #'hl-line-mode)

;; mouse settings
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; frame settings
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 45))
  (add-to-list 'default-frame-alist '(width . 120)))

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

;; vc settings
(setq vc-handled-backends '(Git))

;; commands & functions & definitions

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

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

(defun indent-format-buffer ()
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (indent-region (point-min) (point-max) nil)))

(defun reload-emacs ()
  (interactive)
  (load (locate-user-emacs-file "init.el") :no-error-if-file-is-missing))

;; Load packages
(setq packages '("setup.el" "builtins.el" "packages.el" "langs.el" "color-themes.el"))
(dolist (package packages)
  (tt (format "*** %s" package)
      (load (concat user-emacs-directory "packages/" package))))

;; custom settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-missing)
