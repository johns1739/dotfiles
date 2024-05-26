(use-package ace-window
  ;; Jump to a window
  :defer t
  :bind  (([remap other-window] . ace-window)
          ([remap delete-window] . ace-delete-window)
          ([remap delete-other-windows] . ace-delete-other-windows)
          ([remap window-swap-states] . ace-swap-window)
          ([remap evil-window-next] . ace-window)))

(use-package avy
  :defer t
  :bind (:map goto-map
         ("l" . avy-goto-line)
         ("w" . avy-goto-char-2)))
