;; Utilities
(setq apropos-do-all t)
(setq global-auto-revert-non-file-buffers t)
(setq read-process-output-max (* 1024 1024))
(setq tab-always-indent 'complete)
(setq ring-bell-function 'ignore)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(desktop-save-mode 1)
(electric-indent-mode 1)
(electric-pair-mode -1)
(global-auto-revert-mode t)
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)


;; Garbage collection
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 16 1000 1000)) ;; 16 MB


;; Versioning files and backups
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; Compilation
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)


;; Spacing
(setq require-final-newline t)
(add-hook 'before-save-hook #'whitespace-cleanup)


;; Look and feel
(setq confirm-kill-emacs 'y-or-n-p)
(setq eldoc-echo-area-use-multiline-p nil)
(setq inhibit-startup-message t)
(setq max-mini-window-height 0.2)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)
(setq-default fill-column 120)
(setq-default frame-title-format '("%f"))
(setq-default display-fill-column-indicator-column 100)
(setq-default display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(column-number-mode 1)
(global-hl-line-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(window-divider-mode -1)

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 130))


;; Hippie
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line
        try-expand-line-all-buffers
        ;; try-complete-lisp-symbol
        ;; try-complete-lisp-symbol-partially
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill))

(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))

(ad-activate 'hippie-expand)
