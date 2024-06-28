;; Keybindings
(defvar-keymap global-leader-map :doc "Global leader keymap.")
(defvar-keymap toggle-map :doc "Toggle map")
(defvar-keymap git-map :doc "Git map")
(defvar-keymap diagnostics-map :doc "Diagnostics map")
(defvar-keymap notes-map :doc "Notes map")
(defvar-keymap completion-map :doc "Completion map")
(defvar-keymap compilation-map :doc "Compilation map")
(defvar-keymap my-search-map :doc "Project Search Keymap")

(keymap-global-set "M-SPC" global-leader-map)


;; Modes
(auto-save-visited-mode -1) ;; Annoying with whitespace cleanup constantly moving the point
(column-number-mode 1)
(delete-selection-mode 1)
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP
(electric-indent-mode 1)
(electric-pair-mode 1)
(line-number-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(repeat-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(window-divider-mode 1)
(global-auto-revert-mode t)
(global-so-long-mode t)
(global-eldoc-mode 1)


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


;; GC, Garbage Collection
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 16 1000 1000)) ;; 16 MB


;; Saves & Backups
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


;; Text & Spacing
(setq dictionary-server "dict.org")
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)
(global-set-key [remap just-one-space] #'cycle-spacing)
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)


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


;; Look and feel
(setopt use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq eldoc-echo-area-use-multiline-p nil)
(setq inhibit-startup-message t)
(setq max-mini-window-height 0.2)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)
(setq-default fill-column 80)
(setq-default frame-title-format '("%f"))
(setq-default display-fill-column-indicator-column 100)
(setq-default display-line-numbers-type t)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook #'hl-line-mode)


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
