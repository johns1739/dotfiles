(defvar-keymap global-leader-map
  :doc "Leader mapping to be shared by emacs and evil modes."
  "SPC" #'switch-to-buffer
  "=" #'indent-buffer
  "K" #'eldoc

  ;; Open/Toggle
  "o t" #'vterm-toggle
  "o T" #'vterm-named

  ;; Compilation actions
  "c c" #'compile-dwim
  "c i" #'comint
  "c r" #'recompile
  "c a" #'embark-act
  "c E" #'embark-export
  "c n" #'next-error
  "c p" #'previous-error

  ;; Eval Actions
  "x e" #'eval-last-sexp
  "x E" #'eval-print-last-sexp
  "x :" #'eval-expression
  "x b" #'eval-buffer
  "x r" #'eval-region
  "x f" #'eval-defun

  ;; Editor Actions
  "e y" #'project-copy-relative-file-name
  "e Y" #'copy-absolute-file-name
  "e c" #'goto-configs
  "e t" #'load-theme

  ;; Notes
  "n n" #'denote
  "n t" #'org-todo-list
  "n ;" #'denote-journal-extras-new-or-existing-entry
  "n ," #'org-insert-structure-template

  ;; Gotos
  "g f" #'find-file-at-point
  "g d" #'xref-find-definitions
  "g r" #'xref-find-references
  "g w" #'avy-goto-char-2
  "g h" #'move-beginning-of-line
  "g l" #'move-end-of-line
  "g n" #'next-buffer
  "g p" #'previous-buffer
  "g u" #'goto-address-at-point

  ;; Find
  "f d" #'project-find-dir
  "f f" #'project-find-file
  "f g" #'consult-git-grep
  "f i" #'consult-imenu
  "f I" #'consult-imenu-multi
  "f w" #'consult-line
  "f W" #'consult-line-multi
  "f m" #'consult-mark
  "f M" #'consult-global-mark
  "f n" #'consult-notes
  "f N" #'consult-notes-search-in-all-notes
  "f p" #'project-switch-project
  "f o" #'consult-outline
  "f s" #'consult-ripgrep

  ;; Git
  "j b" #'magit-blame-addition
  "j i" #'diff-hl-show-hunk
  "j j" #'magit-status
  "j J" #'magit-file-dispatch
  "j l" #'magit-buffer-log
  "j n" #'diff-hl-next-hunk
  "j p" #'diff-hl-previous-hunk
  "j s" #'diff-hl-stage-dwim
  "j X" #'diff-hl-revert-hunk
  "j y" #'git-link

  ;; Diagnostics
  "k s" #'consult-flymake
  "k k" #'flymake-show-buffer-diagnostics
  "k K" #'flymake-show-project-diagnostics
  "k n" #'flymake-goto-next-error
  "k p" #'flymake-goto-prev-error

  ;; Completions
  "i ." #'completion-at-point
  "i :" #'cape-emoji
  "i a" #'cape-abbrev
  "i d" #'cape-dabbrev
  "i e" #'cape-elisp-block
  "i f" #'cape-file
  "i h" #'cape-history
  "i k" #'cape-keyword
  "i l" #'cape-line
  "i s" #'cape-elisp-symbol
  "i t" #'completion-tag
  "i w" #'cape-dict
  )

(keymap-global-set "M-SPC" global-leader-map)

(with-eval-after-load 'evil
  (bind-keys*
   ([remap find-file-at-point] . evil-find-file-at-point-with-line)
   :map evil-normal-state-map
   ("g d" . xref-find-definitions)
   ("g h" . evil-beginning-of-line)
   ("g l" . evil-end-of-line)
   ("g n" . next-buffer)
   ("g p" . previous-buffer)
   ("g r" . xref-find-references)
   ("g u" . goto-address-at-point)
   ("g w" . avy-goto-char-2))
  (keymap-set evil-visual-state-map "SPC" global-leader-map)
  (keymap-set evil-normal-state-map "SPC" global-leader-map))

(bind-keys*
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
 ([remap other-window] . ace-window)
 ([remap evil-window-next] . ace-window)
 ([remap load-theme] . consult-theme)

 ;; Extended emacs core
 ("C-h B" . embark-bindings)
 ("<home>" . next-buffer)
 ("<end>" . previous-buffer)

 :map isearch-mode-map
 ("M-s l" . consult-line)
 ("M-s M-l" . consult-line-multi)

 :map minibuffer-local-map
 ("M-s" . consult-history)
 ("M-r" . consult-history)

 :map corfu-map
 ("RET" . nil))

(defun flycheck-set-bindings ()
  (bind-keys :map (current-local-map)
             ([remap consult-flymake] . consult-flycheck)
             ([remap flymake-show-buffer-diagnostics] . flycheck-list-errors)
             ([remap flymake-show-project-diagnostics] . nil)
             ([remap flymake-goto-next-error] . flycheck-next-error)
             ([remap flymake-goto-prev-error] . flycheck-previous-error)))
(add-hook 'flycheck-mode-hook #'flycheck-set-bindings)

(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format-buffer)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)

(defun lsp-set-bindings ()
  "Inject lsp bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . lsp-format-buffer)
             ([remap evil-lookup] . lsp-describe-thing-at-point)
             ([remap eldoc] . lsp-describe-thing-at-point)
             ([remap xref-find-references] . lsp-find-references)
             ([remap xref-find-definitions] . lsp-find-definition)
             ([remap evil-goto-definition] . lsp-find-definition)))
(add-hook 'lsp-managed-mode-hook #'lsp-set-bindings)

(defun ruby-set-bindings ()
  "Inject ruby specific keybindings"
  (bind-keys :map (current-local-map)
             ([remap compile-dwim] . rails-compile)
             ([remap comint] . rails-comint)))
(add-hook 'ruby-ts-mode-hook #'ruby-set-bindings)
