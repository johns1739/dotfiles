;; Completion
(customize-set-variable 'tab-always-indent t)
(setq completion-cycle-threshold 5)
(setq completions-detailed t)
(setq completion-category-overrides '((file (styles . (partial-completion)))))
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-list
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-list-all-buffers
        try-expand-line-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)
(fido-vertical-mode 1)

;; Emacs
(setq ring-bell-function 'ignore)
(setq apropos-do-all t)
(setq read-process-output-max (* 1024 1024))


;; Spell checking
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))


;; History
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq recentf-max-saved-items 50)
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)


;; GC, Garbage Collection
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 16 1000 1000)) ;; 16 MB


;; Backups
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
(let ((backup-dir (locate-user-emacs-file "backups")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir))
  (setq backup-directory-alist `(("." . ,backup-dir))))


;; Compilation
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)


;; Text & Spacing
(setq dictionary-server "dict.org")
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(add-hook 'before-save-hook #'whitespace-cleanup)


;; Scrolling
(setq auto-window-vscroll nil)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)


;; File matching
(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1)))


;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)


;; Buffers
(setq global-auto-revert-non-file-buffers t)
(setq ibuffer-old-time 24)
(setq eshell-scroll-to-bottom-on-output 'this)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(global-auto-revert-mode t)
(global-so-long-mode t)
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Dictionary\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))


;; Eglot
(defalias 'lsp-ensure-caller #'eglot-ensure "Lsp command to call in major modes.")
(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format-buffer)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)


;; Treesitter
;; Install grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (haskell "https://github.com/tree-sitter/haskell-tree-sitter")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (sql "https://github.com/DerekStride/tree-sitter-sql"))))


;; Look and feel
(setopt use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq eldoc-echo-area-use-multiline-p nil)
(setq inhibit-startup-message t)
(setq max-mini-window-height 0.2)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)
(setq-default fill-column 120)
(setq-default frame-title-format '("%f"))
(setq-default display-fill-column-indicator-column 100)
(setq-default display-line-numbers-type 'relative)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook #'hl-line-mode)
(column-number-mode -1)
(line-number-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Org
(defvar notes-directory (locate-user-emacs-file "notes"))
(unless (file-exists-p notes-directory)
  (make-directory notes-directory))
(setq org-directory notes-directory)
(setq org-agenda-files `(,org-directory))
(setq org-log-done 'time)
(setq org-hide-leading-stars t)
(setq org-special-ctrl-a/e t)
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "TODO" "BUILDING" "PULLREQUEST" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO" . "goldenrod1")
        ("BUILDING" . "green2")
        ("PULLREQUEST" . "green4")
        ("DONE" . "SlateGray4")
        ("CANCELED" . "SlateGray4")))
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (shell . t)))

;; Commands
(defun goto-configs ()
  "Go to emacs configs."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))

(defun copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (relative-file-name)))

(defun copy-absolute-file-name ()
  "Copy absolute file path of current buffer."
  (interactive)
  (kill-new (absolute-file-name)))

(defun compile-dwim ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

(defun comint ()
  (interactive)
  (universal-argument)
  (command-execute #'compile-dwim))

(defun vterm-named ()
  (interactive)
  (vterm (read-string "Session name: ")))

(defun project-directory ()
  "Current project directory."
  (project-root (project-current)))

(defun relative-file-name ()
  "Relative from project or cwd directory."
  (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))

(defun absolute-file-name ()
  "Absolute path to file."
  (expand-file-name (buffer-file-name)))

(defun ffap-project-match-1 (name)
  (let ((filename (match-string 1 name)))
    (if (project-current)
        (expand-file-name filename (project-directory))
      (expand-file-name filename default-directory))))


;; Keymaps
(defvar-keymap diagnostics-map :doc "Diagnostics map")
(defvar-keymap notes-map :doc "Notes map")
(defvar-keymap compilation-map :doc "Compilation map")
(defvar-keymap completion-map :doc "Completion map")
(defvar-keymap git-map :doc "Git map")
(defvar-keymap toggle-map :doc "Toggle map")
(defvar-keymap global-leader-map
  :doc "Global leader map"
  "g" goto-map
  "s" search-map
  "i" completion-map
  "c" compilation-map
  "n" notes-map
  "d" diagnostics-map
  "j" git-map
  "o" toggle-map
  "p" project-prefix-map)

(if (display-graphic-p)
    (keymap-global-set "C-;" global-leader-map)
  (keymap-global-set "M-SPC" global-leader-map))
(keymap-global-set "M-i" completion-map)

;; Keybindings
(repeat-mode 1)
(bind-keys*
 ("C-z" . nil) ;; Unbind suspend-frame
 ("M-J" . join-line)
 ("C-o" . pop-global-mark)
 ("M-o" . other-window)
 ("M-/" . hippie-expand) ;; Do not remap dabbrev-expand

 :map global-leader-map
 ("SPC" . project-switch-to-buffer)
 ("TAB" . indent-buffer)
 ("<tab>" . indent-buffer)

 :map completion-map
 ("i" . completion-at-point)
 ("M-i" . completion-at-point)
 ("." . dabbrev-completion)

 :map compilation-map
 ("!" . project-shell-command)
 ("." . eval-defun)
 ("v" . eval-region)
 ("b" . eval-buffer)
 ("c" . compile-dwim)
 ("i" . comint)
 ("r" . recompile)

 :map diagnostics-map
 ("n" . flymake-goto-next-error)
 ("p" . flymake-goto-prev-error)
 ("." . flymake-show-diagnostic)
 ("l" . flymake-show-buffer-diagnostics)
 ("P" . flymake-show-project-diagnostics)

 :repeat-map diagnostics-repeat-map
 ("n" . flymake-goto-next-error)
 ("p" . flymake-goto-prev-error)
 ("." . flymake-show-diagnostic)

 :map notes-map
 (";" . scratch-buffer)
 ("t" . org-todo-list)
 ("a" . org-agenda)
 ("y" . copy-relative-file-name)
 ("Y" . copy-absolute-file-name)

 :map goto-map
 ("n" . next-error)
 ("p" . previous-error)
 ("N" . next-buffer)
 ("P" . previous-buffer)
 ("g" . beginning-of-buffer)
 ("G" . end-of-buffer)
 ("SPC" . switch-to-buffer)
 ("," . goto-configs)
 (":" . goto-line)
 (";" . scratch-buffer)
 ("b" . bookmark-jump)
 ("f" . find-file-at-point)
 ("k" . eldoc)
 ("K" . dictionary-lookup-definition)
 ("d" . xref-find-definitions)
 ("r" . xref-find-references)
 ("u" . goto-address-at-point)

 :map search-map
 ("SPC" . project-switch-to-buffer)
 ("M-s" . project-find-regexp)
 ("s" . project-find-regexp)
 ("S" . rgrep)
 ("i" . imenu)
 ("r" . recentf-open)
 ("f" . project-find-file)
 ("d" . project-find-dir)
 ("g" . grep)
 ("p" . project-switch-project)

 ;; TODO: Why doesn't repeat scrolling work?
 :repeat-map scroll-page-repeat-map
 ("n" . scroll-up-command)
 ("p" . scroll-down-command)

 :repeat-map buffer-navigation-repeat-map
 ("n" . next-buffer)
 ("p" . previous-buffer)

 :repeat-map isearch-repeat-map
 ("s" . isearch-repeat-forward)
 ("r" . isearch-repeat-backward)
 ("n" . isearch-repeat-forward)
 ("p" . isearch-repeat-backward))

(if use-emacs-core-only
  (load-theme 'modus-vivendi t nil))
