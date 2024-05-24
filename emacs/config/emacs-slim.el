;; Completion
(electric-pair-mode 1)
(fido-vertical-mode 1)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'completions-detailed t)
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

;; Hippie
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-list
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-list-all-buffers
        try-expand-line-all-buffers
        ;; try-complete-lisp-symbol-partially
        ;; try-complete-lisp-symbol
        ))

(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)


;; Utilities
(setq apropos-do-all t)
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)


;; Tracking history
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq recentf-max-saved-items 50)
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP
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


;; Spacing
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(electric-indent-mode 1)
(add-hook 'before-save-hook #'whitespace-cleanup)


;; Scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)


;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)


;; Buffers
(setq global-auto-revert-non-file-buffers t)
(setq ibuffer-old-time 24)
(setq eshell-scroll-to-bottom-on-output 'this)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(global-auto-revert-mode t)
(global-so-long-mode t)


;; Look and feel
(setopt use-short-answers t)
(setq confirm-kill-emacs nil)
(setq eldoc-echo-area-use-multiline-p nil)
(setq inhibit-startup-message t)
(setq max-mini-window-height 0.2)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)

(column-number-mode -1)
(line-number-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Keybindings
(bind-keys*
 ("M-J" . join-line)
 ("C-o" . pop-global-mark)
 ("M-o" . other-window)
 ("M-i" . completion-at-point)

 :map goto-map
 ("SPC" . project-switch-project)
 (";" . scratch-buffer)
 ("b" . bookmark-jump)
 ("f" . find-file-at-point)
 ("k" . eldoc)
 ("n" . next-error)
 ("p" . previous-error)
 ("N" . next-buffer)
 ("P" . previous-buffer)
 ("u" . goto-addreess-at-point)

 :map search-map
 ("SPC" . switch-to-buffer)
 ("s" . rgrep)
 ("i" . imenu)
 ("r" . 'query-replace-regexp))

;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Dictionary\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

(load-theme 'modus-vivendi t)
