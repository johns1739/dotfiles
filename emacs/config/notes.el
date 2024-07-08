(use-package emacs
  :init
  (defvar notes-directory (locate-user-emacs-file "notes"))
  (defun relative-file-name ()
    "Relative from project or cwd directory."
    (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))
  (defun absolute-file-name ()
    "Absolute path to file."
    (expand-file-name (buffer-file-name)))
  (defun copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (kill-new (relative-file-name)))
  (defun copy-absolute-file-name ()
    "Copy absolute file path of current buffer."
    (interactive)
    (kill-new (absolute-file-name)))
  :bind (:map notes-map
              (";" . scratch-buffer)
              ("y" . copy-relative-file-name)
              ("Y" . copy-absolute-file-name))
  :config
  (unless (file-exists-p notes-directory)
    (make-directory notes-directory)))

(use-package emacs ;; org
  :init
  (defun org-mode-setup ()
    (electric-indent-local-mode -1))
  :hook (org-mode . org-mode-setup)
  :bind (:map notes-map
              ("," . org-capture-goto-last-stored)
              ("a" . org-agenda)
              ("c" . org-capture)
              ("j" . org-capture-goto-target)
              ("l" . org-store-link)
              ("L" . org-insert-link))
  :custom
  (org-directory notes-directory)
  (org-agenda-files `(,org-directory))
  (org-log-done 'time)
  (org-return-follows-link nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-log-into-drawer t)
  (org-tag-faces '(("bug"  . "sienna")
                   ("feature" . "goldenrod")
                   ("ticket" . "khaki")))
  (org-capture-templates `(("t" "Task"
                            entry (file ,(locate-user-emacs-file "notes/tasks.org"))
                            "* TODO %? %^g\n%t\n%i"
                            :prepend t
                            :empty-lines 1)
                           ("j" "Journal"
                            entry (file ,(locate-user-emacs-file "notes/journal.org"))
                            "* %? %^g\n%t\n%i"
                            :prepend t
                            :empty-lines 1)))
  (org-todo-keyword-faces '(("BACKLOG" . "dark slate gray")
                            ("TODO" . "goldenrod")
                            ("BUILDING" . "khaki")
                            ("PULLREQUEST" . "forest green")
                            ("DONE" . "dark olive green")
                            ("CANCELED" . "sienna")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t))))


;; (with-eval-after-load 'meow
;;   (defvar-keymap meow-org-motion-keymap
;;     :doc "Org-mode motion keymap"
;;     :parent meow-normal-state-keymap)

;;   (meow-define-state org-motion
;;     "Org-mode structural motion"
;;     :lighter "[O]"
;;     :keymap meow-org-motion-keymap)

;;   (setq meow-cursor-type-org-motion 'hbar)

;;   (meow-define-keys 'org-motion
;;     '("<escape>" . meow-normal-mode)
;;     '("<tab>" . org-cycle)
;;     '("<return>" . org-insert-item)
;;     '("S-<return>" . org-insert-todo-heading-respect-content)
;;     '("/" . consult-org-heading)

;;     '(", ," . org-toggle-narrow-to-subtree)
;;     '(", b" . org-narrow-to-block)
;;     '(", e" . org-narrow-to-element)
;;     '(", s" . org-narrow-to-subtree)
;;     '(", w" . widen)

;;     '("b" . org-previous-item)
;;     '("B" . org-move-item-up)
;;     '("c" . meow-keypad-start)
;;     '("f" . org-next-item)
;;     '("F" . org-move-item-down)
;;     '("H" . org-promote-subtree)
;;     '("J" . org-move-subtree-down)
;;     '("K" . org-move-subtree-up)
;;     '("L" . org-demote-subtree)
;;     '("n" . org-next-visible-heading)
;;     '("N" . org-next-block)
;;     '("p" . org-previous-visible-heading)
;;     '("P" . org-previous-block)
;;     '("x" . org-mark-subtree)
;;     '("X" . org-cut-subtree))

;;   (defun org-motion-bindings ()
;;     "Inject org-motion bindings."
;;     (bind-keys :map (current-local-map)
;;                ("`" . meow-org-motion-mode)))
;;   (add-hook 'org-mode-hook #'org-motion-bindings))
