;; TODO list
;; If LSP mode loaded, assign gd gr, etc with lsp calls
;; Keybindgs of org / modes to 'm'

(global-set-key (kbd "<escape>") '("Quit" . keyboard-escape-quit))
(global-set-key (kbd "C-x h") '("Previous buffer" . previous-buffer))
(global-set-key (kbd "C-x l") '("Next buffer" . next-buffer))
(global-set-key (kbd "<escape>") '("Quit" . keyboard-escape-quit))
(define-key corfu-map (kbd "C-SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "<return>") nil)

(defvar my-note-taking-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") '("Capture" . org-roam-capture))
    (define-key m (kbd "f") '("Find" . org-roam-node-find))
    (define-key m (kbd "i") '("Insert" . org-roam-node-insert))
    (define-key m (kbd "j") '("Today" . org-roam-dailies-goto-date))
    (define-key m (kbd "l") '("Toggle" . org-roam-buffer-toggle))
    m)
  "Note-taking")

(defvar my-editor-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "I") '("Reload init.el" . my/reload-init))
    (define-key m (kbd "p") '("packages.el" . my/go-to-packages-file))
    (define-key m (kbd "k") '("keymaps.el" . my/go-to-keymaps-file))
    (define-key m (kbd "i") '("init.el" . my/go-to-init-file))
    (define-key m (kbd "R") '("Restart Emacs" . restart-emacs))
    m)
  "Editor")

(defvar my-find-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "B") '("All buffers" . consult-buffer))
    (define-key m (kbd "I") '("Imenus" . consult-imenu-multi))
    (define-key m (kbd "b") '("Buffer" . consult-project-buffer))
    (define-key m (kbd "d") '("Dir" . project-find-dir))
    (define-key m (kbd "g") '("Grep" . project-find-regexp))
    (define-key m (kbd "i") '("Imenu" . consult-imenu))
    (define-key m (kbd "p") '("Project" . project-switch-project))
    (define-key m (kbd "r") '("Recentf" . consult-recent-file))
    (define-key m (kbd "s") '("Search" . consult-ripgrep))
    m)
  "Find")

(defvar my-code-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C") '("Project compile" . project-compile))
    (define-key m (kbd "c") '("Compile" . my/compile))
    (define-key m (kbd "r") '("Recompile" . recompile))
    (define-key m (kbd "y") '("Yank filename" . my/project-copy-relative-file-name))
    m)
  "Code")

(defvar my-git-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "B") '("Show blame" . magit-blame-addition))
    (define-key m (kbd "G") '("Git dispatch buffer" . magit-file-dispatch))
    (define-key m (kbd "b") '("Git blame" . magit-blame))
    (define-key m (kbd "d") '("Git diff" . magit-diff-buffer-file))
    (define-key m (kbd "g") '("Git status" . magit-status))
    (define-key m (kbd "l") '("Git logs" . magit-log-buffer-file))
    (define-key m (kbd "y") '("Git link" . git-link))
    m)
  "Git")

(defvar my-leader-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "SPC") '("Find file" . project-find-file))
    (define-key m (kbd ".") '("Dired" . project-dired))
    (define-key m (kbd "T") '("Terminal" . vterm))
    (define-key m (kbd "t") '("Terminal other" . vterm-other-window))

    (define-key m (kbd "n") (cons "Note-taking"  my-note-taking-keymap))
    (define-key m (kbd "e") (cons "Editor" my-editor-keymap))
    (define-key m (kbd "f") (cons "Find" my-find-keymap))
    (define-key m (kbd "c") (cons "Code" my-code-keymap))
    (define-key m (kbd "g") (cons "Git" my-git-keymap))
    m)
  "Leader")

(define-key evil-normal-state-map (kbd "SPC") my-leader-keymap)
(define-key evil-visual-state-map (kbd "SPC") my-leader-keymap)

;; Hooks

(defun set-my-leader-bindings ()
  "Inject leader keymap of SPC."
  (local-set-key (kbd "SPC") my-leader-keymap))
(add-hook 'magit-mode-hook 'set-my-leader-bindings)

(defun set-lsp-bindings ()
  "Inject lsp bindings."
  (define-key evil-motion-state-local-map (kbd "g r") 'lsp-find-references)
  (define-key evil-motion-state-local-map (kbd "g = =") 'lsp-format-buffer)
  (define-key evil-motion-state-local-map (kbd "g = r") 'lsp-format-region)
  (define-key evil-motion-state-local-map (kbd "g R") 'lsp-rename)
  (define-key evil-motion-state-local-map (kbd "g d") 'lsp-find-definition))
(add-hook 'lsp-mode-hook 'set-lsp-bindings)
