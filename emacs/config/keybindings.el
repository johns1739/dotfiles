(defvar-keymap global-leader-map
  :doc "Leader mapping to be shared by emacs and evil modes."
  "M-SPC" #'consult-buffer
  ;; "SPC" #'project-find-file
  ;; "d" #'project-dired
  ;; "D" #'project-directory
  ;; "e" #'embark-act
  ;; "E" #'embark-export
  ;; "g" #'magit-status
  ;; "G" #'magit-file-dispatch
  ;; "i" #'consult-imenu
  ;; "I" #'consult-imenu-multi
  ;; "j" #'avy-goto-char-2
  ;; "l" #'consult-line
  ;; "L" #'consult-line-multi
  ;; "n" #'consult-notes
  ;; "N" #'consult-notes-search-in-all-notes
  ;; "m" #'consult-mark
  ;; "M" #'consult-global-mark
  ;; "p" #'project-switch-project
  ;; "r" #'consult-register-load
  ;; "R" #'consult-register-store
  ;; "s" #'consult-ripgrep
  ;; "t" #'project-vterm
  ;; "y" #'project-copy-relative-file-name
  ;; "Y" #'git-link
  ;; ";" #'denote-journal-extras-new-or-existing-entry
  ;; ":" #'denote
  ;; "." #'goto-configs
  )
(keymap-global-set "M-SPC" global-leader-map)

(bind-keys
 ([remap Info-search] . consult-info)
 ([remap bookmark-jump] . consult-bookmark)
 ([remap dabbrev-expand] . hippie-expand)
 ([remap goto-line] . consult-goto-line)
 ([remap isearch-edit-string] . consult-isearch-history)
 ([remap project-switch-to-buffer] . consult-project-buffer)
 ([remap repeat-complex-command] . consult-complex-command)
 ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
 ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
 ([remap switch-to-buffer] . consult-buffer)
 ([remap yank-pop] . consult-yank-pop)
 ([remap flycheck-list-errors] . consult-flycheck)
 ([remap other-window] . ace-window)
 ([remap evil-window-next] . ace-window)

 ("C-x D" . project-dired)
 ("C-x g" . magit-status)
 ("C-x G" . magit-status-dispatch)

 ("C-c C-c" . compile)
 ("C-c c" . comint)
 ("C-c e a" . embark-act)
 ("C-c e c" . embark-collect)
 ("C-c e d" . embark-dwim)
 ("C-c e e" . embark-export)
 ("C-c t" . project-vterm)
 ("C-c y" . project-copy-relative-file-name)
 ("C-c Y" . git-link)
 ("C-c ;" . denote-journal-extras-new-or-existing-entry)
 ("C-c :" . denote)

 ("M-s f" . project-find-file)
 ("M-s i" . consult-imenu)
 ("M-s I" . consult-imenu-multi)
 ("M-s l" . consult-line)
 ("M-s L" . consult-line-multi)
 ("M-s m" . consult-mark)
 ("M-s M" . consult-global-mark)
 ("M-s n" . consult-notes)
 ("M-s N" . consult-notes-search-in-all-notes)
 ("M-s p" . project-switch-project)
 ("M-s s" . consult-ripgrep)

 ("M-g f" . find-file-at-point)
 ("M-g j" . avy-goto-char-2)
 ("M-g ;" . goto-configs)

 ("C-h B" . embark-bindings)

 ;; ("M-o" . er/expand-region))

 ("<home>" . previous-buffer)
 ("<end>" . next-buffer)

 :map isearch-mode-map
 ("M-s l" . consult-line)
 ("M-s L" . consult-line-multi)

 :map minibuffer-local-map
 ("M-s" . consult-history)
 ("M-r" . consult-history)

 :map corfu-map
 ("RET" . nil))

(defun flymake-set-bindings ()
  (keymap-local-set "C-c x" #'consult-flymake))
(add-hook 'fymake-mode-hook #'flymake-set-bindings)

(defun flycheck-set-bindings ()
  (keymap-local-set "C-c x" #'consult-flycheck))
(add-hook 'flycheck-mode-hook #'flycheck-set-bindings)

(with-eval-after-load 'eglot
  (bind-keys
   :map eglot-mode-map
   ("C-c =" . eglot-format-buffer)
   ("C-c r" . eglot-rename)
   ("M-s r" . xref-find-references)
   ("M-g d" . xref-find-definition)))

(defun lsp-set-bindings ()
  "Inject lsp bindings."
  (bind-keys :map (current-local-map)
             ("C-c =" . lsp-format-buffer)
             ("C-c r" . lsp-rename)
             ("M-s r" . lsp-find-references)
             ("M-g d" . lsp-find-definition)))
(add-hook 'lsp-managed-mode-hook #'lsp-set-bindings)

(defun ruby-set-bindings ()
  "Inject ruby specific keybindings"
  (bind-keys :map (current-local-map)
             ([remap compile] . rails-compile)
             ([remap comint] . rails-comint)))
(add-hook 'ruby-ts-mode-hook #'ruby-set-bindings)
