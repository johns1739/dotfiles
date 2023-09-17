(general-define-key
 "<escape>" '(keyboard-escape-quit :wk "Quit")

 :keymaps 'corfu-map
 "C-SPC" 'corfu-insert-separator
 "RET" nil
 "<return>" nil)

(general-create-definer my/leader-key-def
  :keymaps '(normal insert visual emacs)
  :prefix "SPC")

(my/leader-key-def '(normal visual) 'override
  "SPC" '(project-find-file :wk "Find file")
  "." '(project-dired :wk "Dired")

  "T" '(vterm :wk "Terminal")
  "t" '(vterm-other-window :wk "Terminal other")

  "n" '(:ignore t :wk "Note-taking")
  "n c" '(org-roam-capture :wk "Capture")
  "n f" '(org-roam-node-find :wk "Find")
  "n i" '(org-roam-node-insert :wk "Insert")
  "n j" '(org-roam-dailies-capture-today :wk "Today")
  "n l" '(org-roam-buffer-toggle :wk "Toggle")

  "e" '(:ignore t :wk "Editor")
  "e C" '(my/reload-init :wk "Reload config")
  "e c" '(my/go-to-plugins-file :wk "Config")
  "e k" '(my/go-to-keymaps-file :wk "Keymap")
  "e y" '(my/project-copy-relative-file-name :wk "Yank filename")

  "f" '(:ignore t :wk "Find")
  "f B" '(consult-buffer :wk "All buffers")
  "f I" '(consult-imenu-multi :wk "Imenus")
  "f b" '(consult-project-buffer :wk "Buffer")
  "f d" '(project-find-dir :wk "Dir")
  "f g" '(project-find-regexp :wk "Grep")
  "f i" '(consult-imenu :wk "Imenu")
  "f p" '(project-switch-project :wk "Project")
  "f r" '(consult-recent-file :wk "Recentf")
  "f s" '(consult-ripgrep :wk "Search")

  "c" '(:ignore t :wk "Code")
  "c C" '(project-compile :wk "Project compile")
  "c c" '(my/compile :wk "Compile")
  "c r" '(recompile :wk "Recompile")

  "l" '(:ignore t :wk "LSP")
  "l d" '(lsp-find-definition :wk "Definition")
  "l r" '(lsp-find-references :wk "References")
  "l f" '(lsp-format-buffer :wk "Format")

  "g" '(:ignore t :wk "Git")
  "g B" '(magit-blame-addition :wk "Show blame")
  "g G" '(magit-file-dispatch :wk "Git dispatch buffer")
  "g b" '(magit-blame :wk "Git blame")
  "g d" '(magit-diff-buffer-file :wk "Git diff")
  "g g" '(magit-status :wk "Git status")
  "g l" '(magit-log-buffer-file :wk "Git logs")
  "g y" '(git-link :wk "Git link")
  )
