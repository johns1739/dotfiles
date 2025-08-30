;;-*- lexical-binding: t; -*-

(global-set-key [remap backward-sentence] #'backward-sexp)
(global-set-key [remap forward-sentence] #'forward-sexp)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-i") #'completion-at-point)
(global-set-key (kbd "M-I") #'hippie-expand)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-p") #'backward-paragraph)

(define-key goto-map (kbd "SPC") #'switch-to-buffer)
(define-key goto-map (kbd "f") #'find-file-at-point)
(define-key goto-map (kbd "m") #'bookmark-jump)
(define-key goto-map (kbd "M") #'bookmark-set)
(define-key search-map (kbd "s") #'rgrep)

(setq use-short-answers t)
(setq recentf-auto-cleanup 300)
(setq recentf-max-saved-items 100)
(setq history-delete-duplicates t)
(setq isearch-wrap-pause 'no)
(setq completion-styles '(basic substring partial-completion))
(setq completion-category-overrides '((file (styles . (basic partial-completion)))))
(setq completion-cycle-threshold 3)
(setq completions-detailed t)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; hippie settings
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

;; backup settings
(setq create-lockfiles nil)
(let ((backup-dir (locate-user-emacs-file "backups")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir))
  (setq backup-directory-alist `(("." . ,backup-dir))))

;; modes
(recentf-mode 1)
(save-place-mode t)
(savehist-mode t)
(fido-mode 1)

;; custom settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-is-missing)
