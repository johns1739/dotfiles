(keymap-set global-leader-map "g" goto-map)

(bind-keys*
 :map goto-map
 ("SPC" . switch-to-buffer)
 ("." . xref-find-definitions)
 ("," . xref-go-back)
 ("?" . xref-find-references)
 ("/" . xref-find-apropos)
 (";" . goto-configs)
 (":" . goto-line)
 ("%" . xref-find-references-and-replace)
 ("D" . eldoc)
 ("G" . end-of-buffer)
 ("f" . find-file-at-point)
 ("g" . beginning-of-buffer)
 ("n" . next-error)
 ("p" . previous-error)
 ("u" . goto-address-at-point))

(use-package ace-window
  ;; Jump to a window
  :defer t
  :bind  (([remap other-window] . ace-window)
          ;; ([remap delete-window] . ace-delete-window)
          ;; ([remap delete-other-windows] . ace-delete-other-windows)
          ([remap window-swap-states] . ace-swap-window)
          ([remap evil-window-next] . ace-window)))

(use-package avy
  :defer t
  :bind (:map goto-map
         ("l" . avy-goto-line)
         ("w" . avy-goto-char-2)))
