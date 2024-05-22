;; Better default emacs keybindings

(bind-keys*
 ("M-J" . join-line)
 ("C-o" . pop-global-mark)
 ("M-o" . other-window))

(defvar-keymap go-to-map
  :doc "GoTo Keymap"
  "g" #'beginning-of-buffer
  "G" #'end-of-buffer
  "M-g" #'goto-line
  "SPC" #'project-switch-project
  "," #'goto-configs
  ":" #'goto-line
  ";" #'scratch-buffer
  "b" #'bookmark-jump
  "f" #'find-file-at-point
  "d" #'xref-find-definitions
  "k" #'eldoc
  "l" #'avy-goto-line
  "m" #'consult-global-mark
  "n" #'next-error
  "p" #'previous-error
  "N" #'next-buffer
  "P" #'previous-buffer
  "r" #'xref-find-references
  "w" #'avy-goto-char-2
  "u" #'goto-address-at-point)
(keymap-global-set "M-g" go-to-map)

(defvar-keymap project-search-map
  :doc "Project Search Keymap"
  "SPC" #'switch-to-buffer
  "s" #'consult-ripgrep
  "M-s" #'consult-ripgrep
  "S"  #'rg
  "." #'rg-dwim
  "l" #'consult-line
  "L" #'consult-keep-lines
  "i" #'consult-imenu
  "m" #'consult-mark
  "o" #'consult-outline
  "r" #'query-replace-regexp
  "R" #'project-query-replace-regexp
  "f" #'project-find-file
  "d" #'project-find-dir
  "g" #'consult-git-grep)
(keymap-global-set "M-s" project-search-map)

(defvar-keymap completions-map
  :doc "Completions map"
  "i" #'completion-at-point
  "M-i" #'completion-at-point
  "." #'cape-dabbrev
  "a" #'cape-abbrev
  "e" #'cape-elisp-block
  "f" #'cape-file
  "h" #'cape-history
  "k" #'cape-keyword
  "l" #'cape-line
  "s" #'cape-elisp-symbol
  "t" #'completion-tag
  "d" #'cape-dict)
(keymap-global-set "M-i" completions-map)

(defvar-keymap compilation-map
  :doc "Compilation map"
  "c" #'compile-dwim
  "i" #'comint
  "r" #'recompile
  "s" #'consult-compile-error
  "t" #'vterm)

(defvar-keymap notes-map
  :doc "Notes map"
  "n" #'consult-notes
  ";" #'scratch-buffer
  "c" #'denote
  "j" #'denote-journal-extras-new-or-existing-entry
  "t" #'org-todo-list
  "a" #'org-agenda
  "s" #'consult-notes-search-in-all-notes
  "y" #'copy-relative-file-name
  "Y" #'copy-absolute-file-name)

(defvar-keymap git-map
  :doc "Git map"
  "j" #'magit-status
  "." #'diff-hl-show-hunk
  "b" #'magit-blame-addition
  "J" #'magit-file-dispatch
  "l" #'magit-log-buffer-file
  "n" #'diff-hl-next-hunk
  "p" #'diff-hl-previous-hunk
  "S" #'diff-hl-stage-dwim
  "K" #'diff-hl-revert-hunk
  "y" #'git-link)

(defvar-keymap diagnostics-map
  :doc "Diagnostics map"
  "k" #'consult-flymake
  "." #'flymake-show-buffer-diagnostics
  "P" #'flymake-show-project-diagnostics
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

(defvar-keymap popper-map
  :doc "Popper map"
  "o" #'popper-toggle
  "O" #'popper-toggle-type
  "n" #'popper-cycle
  "p" #'popper-cycle-backwards
  "k" #'popper-kill-latest-popup)

(defvar-keymap global-leader-map
  :doc "Leader map"
  "SPC" #'switch-to-buffer
  "TAB" #'indent-buffer
  "<tab>" #'indent-buffer

  "A" #'embark-act
  "E" #'embark-export

  "g" go-to-map
  "s" project-search-map
  "i" completions-map
  "c" compilation-map
  "n" notes-map
  "k" diagnostics-map
  "j" git-map
  "o" popper-map
  )
(keymap-global-set "C-;" global-leader-map)
