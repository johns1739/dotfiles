(defvar notes-directory (locate-user-emacs-file "notes"))
(unless (file-exists-p notes-directory)
  (make-directory notes-directory))

(use-package org
  :defer t
  :init
  (setq org-directory notes-directory)
  (setq org-agenda-files `(,org-directory))
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO" "BUILDING" "PULLREQUEST" "|" "DONE" "CANCELED")))
  (setq org-todo-keyword-faces
        '(("TODO" . "goldenrod1")
          ("BUILDING" . "green2")
          ("PULLREQUEST" . "green4")
          ("DONE" . "SlateGray4")
          ("CANCELED" . "SlateGray4")))
  :custom
  (org-hide-leading-stars t)
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package denote
  :init
  (setq denote-directory notes-directory)
  (setq denote-journal-extras-directory notes-directory))

(use-package consult-notes
  :after denote
  :config
  (consult-notes-denote-mode))
