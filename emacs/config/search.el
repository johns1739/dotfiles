(use-package consult
  :custom
  (register-preview-delay 0.5)
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :bind (([remap Info-search] . consult-info)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap keep-lines] . consult-keep-lines)
         ([remap isearch-edit-string] . consult-isearch-history)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap load-theme] . consult-theme)
         :map diagnostics-map
         ("s" . consult-flymake)
         :map compilation-map
         ("s" . consult-compile-error)
         :map search-map
         ("s" . consult-ripgrep)
         ("M-s" . consult-ripgrep)
         ("c" . consult-compile-error)
         ("I" . consult-imenu-multi)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("o" . consult-outline)
         ("j" . consult-register-load)
         ("J" . consult-register-store)
         ("g" . consult-git-grep))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package rg
  :bind (:map search-map
         ("." . rg-dwim)
         (">" . rg)))