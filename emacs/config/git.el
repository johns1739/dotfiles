(use-package magit
  :defer t
  :config
  (setq magit-list-refs-sortby "-creatordate"))

(use-package git-link
  :defer t)

(use-package diff-hl
  :demand t
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))
