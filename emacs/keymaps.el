;; -*- lexical-binding: t; -*-
;; Anything and all key-binding related logic.

(keymap-global-set "C-x h" '("Previous buffer" . previous-buffer))
(keymap-global-set "C-x l" '("Next buffer" . next-buffer))
(keymap-global-set "M-/" 'hippie-expand)

(defvar my/note-taking-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "c" '("Capture" . org-roam-capture))
    (keymap-set m "f" '("Find" . org-roam-node-find))
    (keymap-set m "i" '("Insert" . org-roam-node-insert))
    (keymap-set m "j" '("Today" . org-roam-dailies-goto-date))
    (keymap-set m "l" '("Store" . org-store-link))
    m)
  "Note Keymap")

(defvar my/editor-settings-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "I" '("Reload init.el" . my/reload-init))
    (keymap-set m "e" '("emacs.el" . my/go-to-emacs-file))
    (keymap-set m "p" '("packages.el" . my/go-to-packages-file))
    (keymap-set m "k" '("keymaps.el" . my/go-to-keymaps-file))
    (keymap-set m "i" '("init.el" . my/go-to-init-file))
    (keymap-set m "R" '("Restart Emacs" . restart-emacs))
    m)
  "Editor Keymap")

(defvar my/search-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "b" '("Search" . consult-bookmark))
    (keymap-set m "i" '("Imenu" . consult-imenu))
    (keymap-set m "j" '("Jump" . avy-goto-char-timer))
    (keymap-set m "g" '("Occur" . occur))
    (keymap-set m "s" '("Search" . consult-line))
    (keymap-set m "r" '("Register" . consult-register))
    m)
  "Search Keymap")

(defvar my/find-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "B" '("All buffers" . consult-buffer))
    (keymap-set m "b" '("Buffer" . consult-project-buffer))
    (keymap-set m "d" '("Dir" . project-find-dir))
    (keymap-set m "g" '("Grep" . project-find-regexp))
    (keymap-set m "i" '("Imenus" . consult-imenu-multi))
    (keymap-set m "n" '("Notes" . org-roam-node-find))
    (keymap-set m "p" '("Project" . project-switch-project))
    (keymap-set m "r" '("Recentf" . consult-recent-file))
    (keymap-set m "s" '("Search" . consult-ripgrep))
    m)
  "Find Keymap")

(defvar my/console-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "C" '("Project compile" . project-compile))
    (keymap-set m "c" '("Compile" . compile))
    (keymap-set m "r" '("Recompile" . recompile))
    (keymap-set m "y" '("Yank filename" . my/project-copy-relative-file-name))
    (keymap-set m "T" '("Terminal" . vterm))
    (keymap-set m "t" '("Terminal other" . vterm-other-window))
    m)
  "Console Keymap")

(defvar my/git-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "B" '("Show blame" . magit-blame-addition))
    (keymap-set m "G" '("Git dispatch buffer" . magit-file-dispatch))
    (keymap-set m "b" '("Git blame" . magit-blame))
    (keymap-set m "d" '("Git diff" . magit-diff-buffer-file))
    (keymap-set m "g" '("Git status" . magit-status))
    (keymap-set m "l" '("Git logs" . magit-log-buffer-file))
    (keymap-set m "y" '("Git link" . git-link))
    m)
  "Git Keymap")

(defvar my/leader-keymap
  (let ((m (make-sparse-keymap)))
    (keymap-set m "SPC" '("Find file" . project-find-file))
    (keymap-set m "." '("Dired" . project-dired))
    (keymap-set m "n" (cons "Notes"  my/note-taking-keymap))
    (keymap-set m "e" (cons "Emacs Settings" my/editor-settings-keymap))
    (keymap-set m "f" (cons "Find" my/find-keymap))
    (keymap-set m "c" (cons "Console" my/console-keymap))
    (keymap-set m "g" (cons "Git" my/git-keymap))
    (keymap-set m "s" (cons "Search" my/search-keymap))
    m)
  "Leader Keymap")

(keymap-global-set "C-SPC" my/leader-keymap)
(keymap-set evil-normal-state-map "SPC" my/leader-keymap)
(keymap-set evil-visual-state-map "SPC" my/leader-keymap)


;;;; LSP Bindings (eglot vs lsp-mode)

(defun my/set-eglot-bindings ()
  "Inject eglot bindings."
  (evil-local-set-key 'normal "g==" 'eglot-format-buffer)
  (evil-local-set-key 'normal "gR" 'eglot-rename))
(add-hook 'eglot-managed-mode-hook 'my/set-eglot-bindings)

(defun my/set-lsp-bindings ()
  "Inject lsp bindings."
  (evil-local-set-key 'normal "g==" 'lsp-format-buffer)
  (evil-local-set-key 'normal "gr" 'lsp-find-references)
  (evil-local-set-key 'normal "g=r" 'lsp-format-region)
  (evil-local-set-key 'normal "gR" 'lsp-rename)
  (evil-local-set-key 'normal "gd" 'lsp-find-definition)
  (evil-local-set-key 'normal "K" 'eldoc))
(add-hook 'lsp-mode-hook 'my/set-lsp-bindings)

(defun my/set-ruby-keybindings ()
  "Inject ruby specific keybindings"
  (keymap-set evil-normal-state-local-map "SPC c c" '("Compile" . my/rails-compile))
  (keymap-set evil-normal-state-local-map "SPC c C" '("Comint" . my/rails-compile-comint)))
(add-hook 'ruby-ts-mode-hook 'my/set-ruby-keybindings)
