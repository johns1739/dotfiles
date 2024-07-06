(keymap-set global-leader-map "j" git-map)

(use-package magit
  :defer t
  :commands (magit-status)
  :bind (:map git-map
              ("j" . magit-status)
              ("m" . magit-blame-addition)
              ("f" . magit-file-dispatch)
              ("l" . magit-log-buffer-file))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m")))
  :config
  ;; (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-list-refs-sortby "-creatordate"))

(use-package git-link
  :defer t
  :bind (:map git-map
              ("y" . git-link)))

(use-package diff-hl
  :demand t
  :bind (:map git-map
              ("." . diff-hl-show-hunk)
              ("n" . diff-hl-next-hunk)
              ("p" . diff-hl-previous-hunk)
              ("S" . diff-hl-stage-dwim)
              ("K" . diff-hl-revert-hunk))
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
