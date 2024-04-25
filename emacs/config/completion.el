(setq tab-always-indent 'complete)

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
        try-complete-file-name
        try-complete-file-name-partially
        try-expand-whole-kill))

(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))

(ad-activate 'hippie-expand)


(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  :straight (corfu :files (:defaults "extensions/*.el") :includes (corfu-echo))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-delay 0.2) ; Enable auto completion
  (corfu-auto-prefix 3) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-echo-delay 0.3)
  (corfu-preselect 'valid)
  (corfu-separator ?\s)
  :config
  (global-corfu-mode 1)
  (corfu-echo-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :custom
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-dict
         #'cape-file
         #'cape-line
         )))
