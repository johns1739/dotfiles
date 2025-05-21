;;-*- lexical-binding: t; -*-

(bind-keys :map global-map
           ("C-x C-b" . ibuffer)
           ("C-z" . nil) ;; unbind suspend-frame
           ([remap backward-sentence] . backward-sexp)
           ([remap forward-sentence] . forward-sexp)
           ([remap split-window-below] . split-window-below-and-jump)
           ([remap split-window-right] . split-window-right-and-jump)
           ("M-n" . forward-paragraph)
           ("M-p" . backward-paragraph)
           ("M-o" . other-window)
           ("M-RET" . comment-indent-new-line)
           ("M-i" . completion-at-point)
           ("M-I" . hippie-expand)
           ("M-L" . duplicate-line)
           ("M-T" . transpose-lines)

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
           ("h" . eldoc)
           ("j" . jump-to-register)
           ("J" . point-to-register)
           ("m" . bookmark-jump)
           ("M" . bookmark-set)
           ("u" . goto-address-at-point)

           :map search-map
           ("SPC" . project-switch-to-buffer)
           ("f" . project-find-file)
           ("g" . rgrep)
           ("l" . occur)
           ("i" . imenu)
           ("k" . keep-lines)
           ("K" . delete-matching-lines)
           ("s" . project-find-regexp)
           ("r" . recentf-open)

           :map help-map
           ("h" . nil))

;; isearch settings
(setq isearch-wrap-pause 'no)

;; duplicating line settings
(setq duplicate-line-final-position 1)

;; completion settings
(setq completion-auto-help t)
(setq completion-auto-select 'second-tab)
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (basic partial-completion)))))
(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)
(setq completion-styles '(basic substring partial-completion))
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-max-height 20)
(setq tab-always-indent 'complete)
(setq-default tab-width 4)
(setq ring-bell-function 'ignore)
(setq xref-search-program 'ripgrep)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq imenu-max-item-length 80)
(setq display-line-numbers-width 4)

;; backup settings
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



;; commands & functions

(defun split-window-below-and-jump ()
  "Split window below and jump to it."
  (interactive)
  (select-window (split-window-below)))

(defun split-window-right-and-jump ()
  "Split window right and jump to it."
  (interactive)
  (select-window (split-window-right)))

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

(global-display-line-numbers-mode 1)
(fido-mode 1)

;; custom settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-is-missing)
