;; Global Keybingsddings
(keymap-global-set "<remap> <delete-horizontal-space>" #'cycle-spacing)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(keymap-global-set "M-o" #'other-window)


;; Go To
(bind-keys :map goto-map
           ("SPC" . switch-to-buffer)
           ("." . xref-find-definitions)
           ("," . xref-go-back)
           ("?" . xref-find-references)
           ("/" . xref-find-apropos)
           (":" . goto-line)
           ("'" . scratch-buffer)
           ("%" . xref-find-references-and-replace)
           ("f" . find-file-at-point)
           ("h" . eldoc)
           ("j" . jump-to-register)
           ("J" . point-to-register)
           ("n" . next-error)
           ("p" . previous-error)
           ("u" . goto-address-at-point))
(setq eldoc-echo-area-use-multiline-p nil)
(setq next-error-recenter '(4))
(setq next-error-highlight 1.0)
(setq next-error-highlight-no-select 1.0)
(setq next-error-message-highlight t)


;; Search
(bind-keys :map search-map
           ("SPC" . project-switch-to-buffer)
           ("." . rgrep)
           ("," . rgrep)
           ("/" . isearch-forward-thing-at-point)
           ("%" . project-query-replace-regexp)
           ("b" . bookmark-jump)
           ("d" . project-find-dir)
           ("f" . project-find-file)
           ("g" . project-find-regexp)
           ("s" . rgrep)
           ("i" . imenu)
           ("o" . occur)
           ("O" . multi-occur)
           ("p" . project-switch-project)
           ("r" . recentf-open))
(setq isearch-wrap-pause 'no)
(setq register-preview-delay 0.5)
(recentf-mode 1)


;; Completion
(bind-keys ("M-i" . completion-at-point)
           ("M-/" . hippie-expand)) ;; Do not remap dabbrev-expand
(setq completion-at-point-functions '(dabbrev-capf))
(setq completion-cycle-threshold 5)
(setq completions-detailed t)
(setq tab-always-indent t)
(setq completion-styles '(basic flex))
(setq completion-category-overrides '((file (styles . (partial-completion)))))
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list '(try-expand-line
                                         try-expand-line-all-buffers
                                         ;; try-expand-whole-kill
                                         try-expand-list
                                         try-expand-list-all-buffers
                                         ;; try-expand-all-abbrevs
                                         try-expand-dabbrev-visible
                                         try-expand-dabbrev
                                         ;; try-expand-dabbrev-from-kill
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(fido-vertical-mode 1)


;; Diagnostics
(defvar-keymap diagnostics-map :doc "Diagnostics map")
(bind-keys :map diagnostics-map
           ("." . flymake-show-diagnostic)
           (";" . flymake-show-buffer-diagnostics)
           ("P" . flymake-show-project-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error)
           :repeat-map diagnostics-repeat-map
           ("." . flymake-show-diagnostic)
           (";" . flymake-show-buffer-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error))
(setq flymake-fringe-indicator-position 'right-fringe)


;; History
(save-place-mode 1)
(savehist-mode 1)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq recentf-max-saved-items 50)
(setq recentf-auto-cleanup 300)


;; Backups & Versioning
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
(let ((backup-dir (locate-user-emacs-file "backups")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir))
  (setq backup-directory-alist `(("." . ,backup-dir))))


;; Modes
(global-auto-revert-mode +1)


;; Editing
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)


;; Scrolling
(setq auto-window-vscroll nil)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)


;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)


;; Buffers
(setq global-auto-revert-non-file-buffers t)
(setq ibuffer-old-time 24)


;; Bells & Whistles
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)
