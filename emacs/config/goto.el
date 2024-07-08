(use-package emacs
  :bind (:map goto-map
              ("SPC" . switch-to-buffer)
              ("." . xref-find-definitions)
              ("," . xref-go-back)
              ("?" . xref-find-references)
              ("/" . xref-find-apropos)
              (";" . goto-configs)
              (":" . goto-line)
              ("%" . xref-find-references-and-replace)
              ("f" . find-file-at-point)
              ("g" . beginning-of-buffer)
              ("G" . end-of-buffer)
              ("k" . eldoc)
              ("n" . next-error)
              ("p" . previous-error)
              ("u" . goto-address-at-point))
  :custom
  (next-error-recenter nil)
  (next-error-highlight 1.0)
  (next-error-highlight-no-select 1.0)
  (next-error-message-highlight t)
  :config
  ;; File matching
  (with-eval-after-load 'ffap
    (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1))))

(use-package ace-window
  :unless use-minimal-emacs
  :defer t
  :bind  (([remap other-window] . ace-window)
          ;; ([remap delete-window] . ace-delete-window)
          ;; ([remap delete-other-windows] . ace-delete-other-windows)
          ([remap window-swap-states] . ace-swap-window)
          ([remap evil-window-next] . ace-window)))

(use-package avy
  :unless use-minimal-emacs
  :defer t
  :bind (:map goto-map
         ("l" . avy-goto-line)
         ("w" . avy-goto-char-2)))
