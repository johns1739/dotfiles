(defvar-keymap global-leader-map
  :doc "Leader mapping to be shared by emacs and evil modes."
  "M-SPC" #'consult-buffer
  "SPC" #'consult-buffer
  "=" #'indent-buffer

  "." #'xref-find-definitions
  "?" #'xref-find-references

  ;; Toggled buffers
  "o o" #'vterm-toggle
  "o O" #'vterm-toggle-cd

  ;; Code actions
  "c c" #'compile
  "c i" #'comint
  "c r" #'recompile
  "c a" #'embark-act
  "c E" #'embark-export
  "c y" #'project-copy-relative-file-name

  ;; Notes
  "n n" #'denote
  "n ;" #'denote-journal-extras-new-or-existing-entry

  ;; Gotos
  "g f" #'find-file-at-point
  "g d" #'project-dired
  "g n" #'next-error
  "g p" #'previous-error
  "g c" #'goto-configs
  "g l" #'consult-goto-line

  ;; Searches
  "s f" #'project-find-file
  "s d" #'project-find-dir
  "s i" #'consult-imenu
  "s I" #'consult-imenu-multi
  "s l" #'consult-line
  "s L" #'consult-line-multi
  "s m" #'consult-mark
  "s M" #'consult-global-mark
  "s n" #'consult-notes
  "s N" #'consult-notes-search-in-all-notes
  "s p" #'project-switch-project
  "s s" #'consult-ripgrep

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
  "k p" #'flymake-goto-prev-error)


(keymap-global-set "M-SPC" global-leader-map)
(with-eval-after-load 'evil
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

 ;; Extended emacs core
 ("C-h B" . embark-bindings)
 ("C-S-j" . avy-goto-char-2)
 ("C-S-m" . mc/mark-all-dwim)
 ("C->" . mc/mark-next-like-this)
 ("C-<" . mc/mark-previous-like-this)
 ("S-<return>" . join-line)
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

(defun flymake-set-bindings ()
  (keymap-local-set "M-SPC x" #'consult-flymake))
(add-hook 'fymake-mode-hook #'flymake-set-bindings)

(defun flycheck-set-bindings ()
  (bind-keys :map (current-local-map)
             ([remap consult-flymake] . consult-flycheck)
             ([remap flymake-show-buffer-diagnostics] . flycheck-list-errors)
             ([remap flymake-show-project-diagnostics] . nil)
             ([remap flymake-goto-next-error] . flycheck-next-error)
             ([remap flymake-goto-prev-error] . flycheck-previous-error)))
(add-hook 'flycheck-mode-hook #'flycheck-set-bindings)

(with-eval-after-load 'eglot
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . lsp-format-buffer)))

(defun lsp-set-bindings ()
  "Inject lsp bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . lsp-format-buffer)
             ([remap xref-find-references] . lsp-find-references)
             ([remap xref-find-definitions] . lsp-find-definition)))
(add-hook 'lsp-managed-mode-hook #'lsp-set-bindings)

(defun ruby-set-bindings ()
  "Inject ruby specific keybindings"
  (bind-keys :map (current-local-map)
             ([remap compile] . rails-compile)
             ([remap comint] . rails-comint)))
(add-hook 'ruby-ts-mode-hook #'ruby-set-bindings)
