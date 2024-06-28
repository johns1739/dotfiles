(keymap-set global-leader-map "s" my-search-map)

(keymap-global-set "<remap> <list-buffers>" #'ibuffer)

(bind-keys*
 :map my-search-map
 ("SPC" . project-switch-to-buffer)
 ("." . rgrep)
 ("," . rgrep)
 ("/" . isearch-forward-thing-at-point)
 ("%" . project-query-replace-regexp)
 ("b" . bookmark-jump)
 ("d" . project-find-dir)
 ("f" . project-find-file)
 ("g" . project-find-regexp)
 ("s" . rgrep)
 ("i" . imenu)
 ("o" . occur)
 ("O" . multi-occur)
 ("p" . project-switch-project)
 ("r" . recentf-open)
 ("t" . load-theme))

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
         ([remap recentf-open] . consult-recent-file)
         :map my-search-map
         ("s" . consult-ripgrep)
         ("c" . consult-compile-error)
         ("h" . consult-outline)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         ("k" . consult-flymake)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("j" . consult-register-load)
         ("J" . consult-register-store)
         ("g" . consult-git-grep))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package rg
  :bind (:map my-search-map
         ("S" . rg)
         ("." . rg-dwim)))
