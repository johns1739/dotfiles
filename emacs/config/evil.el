(defvar-keymap global-leader-map
  :doc "Leader mapping to be shared by emacs and evil modes."
  "SPC" #'project-switch-to-buffer
  "S-SPC" #'switch-to-buffer
  "=" #'indent-buffer
  "K" #'eldoc
  "." #'dired-jump
  ":" #'consult-goto-line
  ";" #'scratch-buffer

  ;; Open / Toggle
  "o ." #'dired-jump
  "o t" #'vterm-toggle
  "o T" #'vterm-named

  ;; Compilation actions
  "c s" #'consult-compile-error
  "c c" #'compile-dwim
  "c i" #'comint
  "c r" #'recompile
  "c n" #'next-error
  "c p" #'previous-error

  ;; Eval Actions
  "x e" #'eval-last-sexp
  "x E" #'eval-print-last-sexp
  "x :" #'eval-expression
  "x b" #'eval-buffer
  "x r" #'eval-region
  "x ." #'eval-defun

  ;; Editor Actions
  "e ." #'embark-dwim
  "e a" #'embark-act
  "e c" #'embark-collect
  "e C" #'embark-export
  "e Y" #'copy-absolute-file-name
  "e ," #'goto-configs
  "e t" #'load-theme
  "e h" #'embark-bindings

  ;; Notes
  "n n" #'denote
  "n j" #'denote-journal-extras-new-or-existing-entry
  "n ;" #'scratch-buffer
  "n t" #'org-todo-list
  "n a" #'org-agenda
  "n f" #'consult-notes
  "n s" #'consult-notes-search-in-all-notes

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

  ;; Buffer Navigation
  "s ." #'isearch-forward-symbol-at-point
  "s s" #'consult-line
  "s i" #'consult-imenu
  "s I" #'consult-imenu-multi
  "s L" #'consult-keep-lines
  "s m" #'consult-mark
  "s M" #'consult-global-mark
  "s o" #'consult-outline
  "s n" #'consult-org-heading ;; TODO: mode specific
  "s N" #'consult-org-agenda  ;; TODO: mode specific

  ;; Find
  "f ." #'rg-dwim
  "f SPC" #'switch-to-buffer
  "f d" #'project-find-dir
  "f f" #'project-find-file
  "f F" #'consult-find
  "f g" #'consult-git-grep
  "f r" #'consult-recent-file
  "f s" #'consult-ripgrep
  "f S" #'rg

  ;; Git
  "j b" #'magit-blame-addition
  "j j" #'magit-status
  "j J" #'magit-file-dispatch
  "j l" #'magit-log-buffer-file
  "j n" #'diff-hl-next-hunk
  "j p" #'diff-hl-previous-hunk
  "j s" #'diff-hl-show-hunk
  "j S" #'diff-hl-stage-dwim
  "j X" #'diff-hl-revert-hunk
  "j y" #'git-link

  ;; Diagnostics
  "k s" #'consult-flymake
  "k k" #'flymake-show-buffer-diagnostics
  "k K" #'flymake-show-project-diagnostics
  "k n" #'flymake-goto-next-error
  "k p" #'flymake-goto-prev-error

  ;; Completions
  "i i" #'hippie-expand
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

  ;; project
  "p ." #'project-dired
  "p y" #'project-copy-relative-file-name
  "p o" #'project-display-buffer
  "p p" #'project-switch-project
  "p K" #'project-kill-buffers
  "p R" #'project-query-replace-regexp
  )

(use-package evil
  :demand t
  :custom
  (evil-lookup-func 'eldoc)
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-motion-state-cursor 'hollow)
  (setq evil-normal-state-cursor 'box)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-w-in-emacs-state t)
  (with-eval-after-load 'evil
    (bind-keys*
     ([remap find-file-at-point] . evil-find-file-at-point-with-line)
     :map evil-normal-state-map
     ("q" . nil)
     ("Q" . evil-record-macro)
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
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
