(defvar-keymap global-leader-map
  :doc "Leader mappings"
  "SPC" #'project-switch-to-buffer
  "=" #'indent-buffer
  "." #'dired
  ":" #'consult-goto-line

  ;; Compilation / Code actions
  "c c" #'compile-dwim
  "c ." #'eval-defun
  "c E" #'embark-export
  "c i" #'comint
  "c n" #'next-error
  "c p" #'previous-error
  "c r" #'recompile
  "c s" #'consult-compile-error
  "c t" #'vterm-toggle

  ;; Note-taking
  "n n" #'denote
  "n j" #'denote-journal-extras-new-or-existing-entry
  "n ;" #'scratch-buffer
  "n t" #'org-todo-list
  "n a" #'org-agenda
  "n f" #'consult-notes
  "n s" #'consult-notes-search-in-all-notes
  "n Y" #'copy-absolute-file-name
  "n y" #'project-copy-relative-file-name

  ;; Buffers
  "b b" #'switch-to-buffer
  "b n" #'next-buffer
  "b p" #'previous-buffer

  ;; Gotos
  "g ," #'goto-configs
  "g f" #'find-file-at-point
  "g d" #'xref-find-definitions
  "g k" #'eldoc
  "g r" #'xref-find-references
  "g w" #'avy-goto-char-2
  "g u" #'goto-address-at-point

  ;; Search within buffer
  "s ." #'isearch-forward-symbol-at-point
  "s s" #'consult-line
  "s i" #'consult-imenu
  "s m" #'consult-mark
  "s o" #'consult-outline

  ;; Find
  "f ." #'rg-dwim
  "f d" #'project-find-dir
  "f f" #'project-find-file
  "f g" #'consult-git-grep
  "f r" #'consult-recent-file
  "f s" #'consult-ripgrep
  "f S" #'rg

  ;; Git
  "j ." #'diff-hl-show-hunk
  "j b" #'magit-blame-addition
  "j j" #'magit-status
  "j J" #'magit-file-dispatch
  "j l" #'magit-log-buffer-file
  "j n" #'diff-hl-next-hunk
  "j p" #'diff-hl-previous-hunk
  "j S" #'diff-hl-stage-dwim
  "j K" #'diff-hl-revert-hunk
  "j y" #'git-link

  ;; Diagnostics
  "k k" #'consult-flymake
  "k ." #'flymake-show-buffer-diagnostics
  "k P" #'flymake-show-project-diagnostics
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

(keymap-global-set "M-SPC" global-leader-map)
