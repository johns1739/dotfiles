(use-package denote
  :bind (:map notes-map
              ("c" . denote)
              ("j" . denote-journal-extras-new-or-existing-entry))
  :init
  (setq denote-directory notes-directory)
  (setq denote-journal-extras-directory notes-directory))

(use-package consult-notes
  :bind (:map notes-map
              ("n" . consult-notes)
              ("N" . consult-notes-search-in-all-notes))
  :config
  (consult-notes-denote-mode))
