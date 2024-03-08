(setq org-directory (expand-file-name "org" user-emacs-directory))
(unless (file-exists-p org-directory)
  (make-directory org-directory))
