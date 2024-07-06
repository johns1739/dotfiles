(keymap-set global-leader-map "k" diagnostics-map)

(bind-keys*
 :map diagnostics-map
 ("." . flymake-show-diagnostic)
 ("k" . flymake-show-buffer-diagnostics)
 ("K" . flymake-show-project-diagnostics)
 ("n" . flymake-goto-next-error)
 ("p" . flymake-goto-prev-error)
 :repeat-map diagnostics-repeat-map
 ("." . flymake-show-diagnostic)
 ("n" . flymake-goto-next-error)
 ("p" . flymake-goto-prev-error))

(setq flymake-fringe-indicator-position 'right-fringe)

(use-package flycheck
  :disabled t
  ;; https://www.flycheck.org/en/latest/
  :defer t
  :bind (:repeat-map flycheck-error-repeat-map
                     ("n" . flycheck-next-error)
                     ("p" . flycheck-previous-error)
                     ("." . flycheck-display-error-at-point))

  :init
  (defun flycheck-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap consult-flymake] . consult-flycheck)
               ([remap flymake-show-diagnostic] . flycheck-display-error-at-point)
               ([remap flymake-show-buffer-diagnostics] . flycheck-list-errors)
               ([remap flymake-show-project-diagnostics] . nil)
               ([remap flymake-goto-next-error] . flycheck-next-error)
               ([remap flymake-goto-prev-error] . flycheck-previous-error)))
  :custom
  (flycheck-indication-mode 'right-fringe)
  :hook
  (flycheck-mode . flycheck-set-bindings))

(use-package consult-flycheck
  :disabled t
  :defer t
  :commands (consult-flycheck))
