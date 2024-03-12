(use-package magit
  :defer t
  :config
  (setq magit-list-refs-sortby "-creatordate"))

(use-package git-link
  :defer t)

(use-package git-gutter
  :disabled
  :config
  (global-git-gutter-mode 1))

(use-package diff-hl
  :demand t
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (set-face-attribute 'diff-hl-insert nil :foreground "SeaGreen4" :background "SeaGreen4")
  (set-face-attribute 'diff-hl-delete nil :foreground "IndianRed4" :background "IndianRed4")
  (set-face-attribute 'diff-hl-change nil :foreground "SteelBlue4" :background "SteelBlue4")
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))
