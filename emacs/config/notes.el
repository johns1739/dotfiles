(keymap-set global-leader-map "n" notes-map)

(bind-keys*
 :map notes-map
 (";" . scratch-buffer)
 ("a" . org-agenda)
 ("l" . org-store-link)
 ("L" . org-insert-last-stored-link)
 ("t" . org-todo-list)
 ("y" . copy-relative-file-name)
 ("Y" . copy-absolute-file-name))

(defvar notes-directory (locate-user-emacs-file "notes"))
(unless (file-exists-p notes-directory)
  (make-directory notes-directory))

(setq org-directory notes-directory)
(setq org-agenda-files `(,org-directory))
(setq org-log-done 'time)
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-special-ctrl-a/e t)
(setq org-startup-indented t)

(setq org-todo-keywords
      '((sequence "TODO" "BUILDING" "PULLREQUEST" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO" . "goldenrod")
        ("BUILDING" . "khaki")
        ("PULLREQUEST" . "forest green")
        ("DONE" . "dark olive green")
        ("CANCELED" . "sienna")))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (shell . t)))
(defun org-mode-setup ()
  (electric-indent-local-mode -1))
(add-hook 'org-mode-hook #'org-mode-setup)

(use-package denote
  :bind (:map notes-map
              ("c" . denote)
              ("j" . denote-journal-extras-new-or-existing-entry)
              ("R" . denote-rename-file-using-front-matter))
  :init
  (setq denote-directory notes-directory)
  (setq denote-journal-extras-directory notes-directory))

(use-package consult-notes
  :bind (:map notes-map
              ("n" . consult-notes)
              :map my-search-map
              ("n" . consult-notes-search-in-all-notes))
  :config
  (consult-notes-denote-mode))
