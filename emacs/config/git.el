(use-package magit
  :defer t
  :commands (magit-status)
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m")))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
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

(use-package magit-todos
  :disabled t ;; Too slow for api-app
  :after magit
  :config
  (magit-todos-mode 1))
