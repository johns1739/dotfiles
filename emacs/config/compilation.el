(keymap-set global-leader-map "c" compilation-map)

(bind-keys*
 :map compilation-map
 ("!" . project-async-shell-command)
 ("." . eval-defun)
 ("b" . eval-buffer)
 ("c" . compile-dwim)
 ("i" . comint)
 ("r" . recompile)
 ("v" . eval-region))

(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
