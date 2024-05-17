(setq goto-map
      (define-keymap
        "g" #'beginning-of-buffer
        "M-g" #'beginning-of-buffer
        "G" #'end-of-buffer
        "SPC" #'project-switch-project
        "," #'goto-configs
        ":" #'goto-line
        ";" #'scratch-buffer
        "f" #'find-file-at-point
        "d" #'xref-find-definitions
        "k" #'eldoc
        "n" #'next-error
        "p" #'previous-error
        "o" #'ace-window
        "N" #'next-buffer
        "P" #'previous-buffer
        "r" #'xref-find-references
        "w" #'avy-goto-char-2
        "u" #'goto-address-at-point))
(keymap-global-set "M-g" goto-map)

;; (setq search-map
;;       (define-keymap
;;         "s" #'consult-line
;;         "M-s" #'consult-line
;;         "SPC" #'consult-project-buffer
;;         "." #'isearch-forward-symbol-at-point
;;         "i" #'consult-imenu
;;         "m" #'consult-mark
;;         "o" #'consult-outline))
(setq search-map
      (define-keymap
        "s" #'consult-ripgrep
        "M-s" #'consult-ripgrep
        "S" #'rg-dwim
        "SPC" #'consult-buffer
        "." #'isearch-forward-symbol-at-point
        "l" #'consult-line
        "L" #'consult-keep-lines
        "b" #'consult-project-buffer
        "i" #'consult-imenu
        "m" #'consult-mark
        "o" #'consult-outline
        "f" #'project-find-file
        "d" #'project-find-dir
        "g" #'consult-git-grep
        "r" #'rg))
(keymap-global-set "M-s" search-map)

(defvar-keymap compilation-map
  :doc "Compilation map"
  "c" #'compile-dwim
  "M-c" #'compile-dwim
  "." #'eval-defun
  ">" #'eval-buffer
  "=" #'indent-buffer
  "E" #'embark-export
  "i" #'comint
  "n" #'next-error
  "p" #'previous-error
  "r" #'recompile
  "s" #'consult-compile-error
  "t" #'vterm-toggle)
(keymap-global-set "M-c" compilation-map)

(defvar-keymap notes-map
  :doc "Notes map"
  "n" #'consult-notes
  "M-n" #'consult-notes
  "c" #'denote
  "j" #'denote-journal-extras-new-or-existing-entry
  ";" #'scratch-buffer
  "t" #'org-todo-list
  "a" #'org-agenda
  "s" #'consult-notes-search-in-all-notes
  "Y" #'copy-absolute-file-name
  "y" #'project-copy-relative-file-name)
(keymap-global-set "M-n" notes-map)

(defvar-keymap buffer-map
  :doc "Buffer map"
  "SPC" #'switch-to-buffer
  "M-SPC" #'switch-to-buffer
  "k" #'kill-this-buffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "r" #'rename-buffer
  "w" #'write-file)
;; (keymap-global-set "M-b" buffer-map)

(defvar-keymap find-map
  :doc "Find map"
  "f" #'project-find-file
  "M-f" #'project-find-file
  "SPC" #'consult-buffer
  "." #'rg-dwim
  "d" #'project-find-dir
  "g" #'consult-git-grep
  "r" #'rg
  "s" #'consult-ripgrep)
;; (keymap-global-set "M-f" find-map)

(defvar-keymap git-map
  :doc "Git map"
  ;Git
  "j" #'magit-status
  "M-j" #'magit-status
  "." #'diff-hl-show-hunk
  "b" #'magit-blame-addition
  "J" #'magit-file-dispatch
  "l" #'magit-log-buffer-file
  "n" #'diff-hl-next-hunk
  "p" #'diff-hl-previous-hunk
  "S" #'diff-hl-stage-dwim
  "K" #'diff-hl-revert-hunk
  "y" #'git-link)
(keymap-global-set "M-j" git-map)

(defvar-keymap diagnostics-map
  :doc "Diagnostics map"
  "k" #'consult-flymake
  "M-k" #'consult-flymake
  "." #'flymake-show-buffer-diagnostics
  "P" #'flymake-show-project-diagnostics
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)
(keymap-global-set "M-k" diagnostics-map)

(defvar-keymap completions-map
  :doc "Completions map"
  "i" #'completion-at-point
  "M-i" #'completion-at-point
  "." #'hippie-expand
  "a" #'cape-abbrev
  "d" #'cape-dabbrev
  "e" #'cape-elisp-block
  "f" #'cape-file
  "h" #'cape-history
  "k" #'cape-keyword
  "l" #'cape-line
  "s" #'cape-elisp-symbol
  "t" #'completion-tag
  "w" #'cape-dict)
(keymap-global-set "M-i" completions-map)

(defvar-keymap project-map
  :doc "Project map"
  "p" #'project-switch-project
  "M-p" #'project-switch-project
  "SPC" #'project-switch-to-buffer
  "." #'project-dired
  "y" #'project-copy-relative-file-name
  "o" #'project-display-buffer
  "K" #'project-kill-buffers
  "%" #'project-query-replace-regexp)
(keymap-global-set "M-p" project-map)

(defvar-keymap global-leader-map
  :doc "Leader map"
  "SPC" #'switch-to-buffer
  "M-SPC" #'switch-to-buffer
  "=" #'indent-buffer
  "g" goto-map
  "f" find-map
  "s" search-map
  "n" notes-map
  "b" buffer-map
  "c" compilation-map
  "i" completions-map
  "k" diagnostics-map
  "j" git-map
  "p" project-map)
(keymap-global-set "M-SPC" global-leader-map)
