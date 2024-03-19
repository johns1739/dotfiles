(use-package org
  :defer t
  :init
  (setq org-directory (locate-user-emacs-file "denote"))
  (setq org-agenda-files `(,org-directory))
  :custom
  (org-hide-leading-stars t)
  (org-special-ctrl-a/e t)
  :config
  (unless (file-exists-p org-directory)
    (make-directory org-directory)))

(use-package denote
  :init
  (setq denote-directory (locate-user-emacs-file "denote"))
  :config
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory)))

(use-package consult-notes
  :after denote
  :config
  (consult-notes-denote-mode))
