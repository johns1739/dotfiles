(use-package orderless
  :init
  (defun orderless-lsp-setup ()
    (setq completion-styles '(basic orderless)
          completion-category-defaults nil))
  :custom
  (completion-styles '(orderless basic))
  :hook
  (lsp-managed-mode . orderless-lsp-setup))

(use-package embark
  :defer t
  :bind (:map global-leader-map
              ("A" . embark-act)
              ("E" . embark-export)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package undo-tree
  :disabled t
  :custom
  (undo-tree-visualizer-timestamps t)
  :config
  (let ((undo-tree-history-directory (locate-user-emacs-file "undo-tree-history")))
    (unless (file-exists-p undo-tree-history-directory)
      (make-directory undo-tree-history-directory))
    (setq undo-tree-history-directory-alist
          `(("." . ,undo-tree-history-directory))))
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package indent-guide
  :hook prog-mode)

;; TODO: Create jump-to-definition-dwim
(use-package dumb-jump
  :disabled t)
