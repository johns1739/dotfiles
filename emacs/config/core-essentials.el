(use-package orderless
  :unless use-minimal-emacs
  :custom
  (completion-styles '(orderless basic)))

(use-package embark
  :unless use-minimal-emacs
  :defer t
  :bind (:map global-leader-map
              ("A" . embark-act)
              ("E" . embark-export)))

(use-package embark-consult
  :unless use-minimal-emacs
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package undo-tree
  :unless use-minimal-emacs
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
  :unless use-minimal-emacs
  :config
  (which-key-mode))

(use-package indent-guide
  :unless use-minimal-emacs
  :hook prog-mode)

;; TODO: Create jump-to-definition-dwim
(use-package dumb-jump
  :unless use-minimal-emacs
  :disabled t)
