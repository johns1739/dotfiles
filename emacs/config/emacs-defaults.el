;; Global Keybindings
(keymap-global-set "<remap> <just-one-space>" #'cycle-spacing)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)

;; Global Leader Keybindings
(defvar-keymap global-leader-map :doc "Global leader keymap.")
(keymap-global-set "M-SPC" global-leader-map)

;; Go To
(keymap-set global-leader-map "g" goto-map)
(bind-keys :map goto-map
           ("SPC" . switch-to-buffer)
           ("." . xref-find-definitions)
           ("," . xref-go-back)
           ("?" . xref-find-references)
           ("/" . xref-find-apropos)
           (";" . goto-configs)
           (":" . goto-line)
           ("%" . xref-find-references-and-replace)
           ("f" . find-file-at-point)
           ("g" . beginning-of-buffer)
           ("G" . end-of-buffer)
           ("k" . eldoc)
           ("n" . next-error)
           ("p" . previous-error)
           ("u" . goto-address-at-point))
(setq eldoc-echo-area-use-multiline-p nil)
(setq next-error-recenter '(4))
(setq next-error-highlight 1.0)
(setq next-error-highlight-no-select 1.0)
(setq next-error-message-highlight t)
(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1)))

;; Search
(keymap-set global-leader-map "s" search-map)
(bind-keys :map search-map
           ("SPC" . project-switch-to-buffer)
           ("." . rgrep)
           ("," . rgrep)
           ("/" . isearch-forward-thing-at-point)
           ("%" . project-query-replace-regexp)
           ("b" . bookmark-jump)
           ("d" . project-find-dir)
           ("f" . project-find-file)
           ("g" . project-find-regexp)
           ("s" . rgrep)
           ("i" . imenu)
           ("n" . org-search-view)
           ("N" . org-occur-in-agenda-files)
           ("o" . occur)
           ("O" . multi-occur)
           ("p" . project-switch-project)
           ("r" . recentf-open)
           ("t" . load-theme))
(setq isearch-wrap-pause 'no)
(setq register-preview-delay 0.5)


;; Diagnostics
(defvar-keymap diagnostics-map :doc "Diagnostics map")
(keymap-set global-leader-map "k" diagnostics-map)
(bind-keys :map diagnostics-map
           ("." . flymake-show-diagnostic)
           ("," . flymake-show-buffer-diagnostics)
           ("P" . flymake-show-project-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error)
           :repeat-map diagnostics-repeat-map
           ("." . flymake-show-diagnostic)
           ("," . flymake-show-buffer-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error))
(setq flymake-fringe-indicator-position 'right-fringe)


;; Compile
(defvar-keymap compilation-map :doc "Compilation map")
(keymap-set global-leader-map "c" compilation-map)
(bind-keys :map compilation-map
           ("!" . project-async-shell-command)
           ("." . eval-defun)
           ("b" . eval-buffer)
           ("c" . compile-dwim)
           ("e" . eval-last-sexp)
           ("E" . eval-print-last-sexp)
           ("g" . recompile)
           ("i" . comint)
           ("r" . eval-region))
(setq compilation-window-height 20)
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(add-hook 'compilation-filter-hook  #'ansi-color-compilation-filter)


;; Notes
(defvar-keymap notes-map :doc "Notes map")
(keymap-set global-leader-map "n" notes-map)
(bind-keys :map notes-map
           ("," . org-capture-goto-last-stored)
           (";" . scratch-buffer)
           ("a" . org-agenda)
           ("c" . org-capture)
           ("j" . org-capture-goto-target)
           ("l" . org-store-link)
           ("L" . org-insert-link)
           ("y" . copy-relative-file-name)
           ("Y" . copy-absolute-file-name))
(defvar notes-directory (locate-user-emacs-file "notes"))
(unless (file-exists-p notes-directory)
  (make-directory notes-directory))
(setq org-directory notes-directory)
(setq org-agenda-files `(,org-directory))
(setq org-log-done 'time)
(setq org-return-follows-link nil)
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-special-ctrl-a/e t)
(setq org-startup-indented t)
(setq org-log-into-drawer t)
(setq org-tag-faces '(("bug"  . "sienna")
                      ("feature" . "goldenrod")
                      ("ticket" . "khaki")))
(setq org-capture-templates `(("t" "Task"
                               entry (file ,(locate-user-emacs-file "notes/tasks.org"))
                               "* TODO %? %^g\n%t\n%i"
                               :prepend t
                               :empty-lines 1)
                              ("j" "Journal"
                               entry (file ,(locate-user-emacs-file "notes/journal.org"))
                               "* %? %^g\n%t\n%i"
                               :prepend t
                               :empty-lines 1)))
(setq org-todo-keyword-faces '(("BACKLOG" . "dark slate gray")
                               ("TODO" . "goldenrod")
                               ("BUILDING" . "khaki")
                               ("PULLREQUEST" . "forest green")
                               ("DONE" . "dark olive green")
                               ("CANCELED" . "sienna")))
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t))))


;; Completion
(defvar-keymap completion-map :doc "Completion map")
(keymap-set global-leader-map "i" completion-map)
(bind-keys ("M-i" . completion-at-point)
           ("M-/" . hippie-expand) ;; Do not remap dabbrev-expand
           :map completion-map
           ("." . dabbrev-completion)
           ("i" . completion-at-point))
(setq completion-at-point-functions '(dabbrev-capf))
(setq completion-cycle-threshold 5)
(setq completions-detailed t)
(setq tab-always-indent t)
(setq completion-styles '(basic flex))
(setq completion-category-overrides '((file (styles . (partial-completion)))))
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list '(try-expand-line
                                         try-expand-line-all-buffers
                                         ;; try-expand-whole-kill
                                         try-expand-list
                                         try-expand-list-all-buffers
                                         ;; try-expand-all-abbrevs
                                         try-expand-dabbrev-visible
                                         try-expand-dabbrev
                                         ;; try-expand-dabbrev-from-kill
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
;;                                     try-expand-all-abbrevs

;;                                     try-expand-dabbrev
;;                                     try-expand-dabbrev-from-kill
;;                                     try-expand-whole-kill
;;                                     try-expand-list
;;                                     try-expand-line
;;                                     try-expand-dabbrev-all-buffers
;;                                     try-expand-list-all-buffers
;;                                     try-expand-line-all-buffers
;;                                     try-complete-file-name-partially
;;                                     try-complete-file-name
;;                                     try-complete-lisp-symbol-partially
;;                                     try-complete-lisp-symbol))
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)
(icomplete-vertical-mode 1)


;; Toggle
(defvar-keymap toggle-map :doc "Toggle map")
(keymap-set global-leader-map "t" toggle-map)
(bind-keys :map toggle-map
           ("b" . toggle-big-font))
(defvar toggle-big-font-sizes '(160 200 240)
  "List of font sizes to toggle between.")


;; Git
(defvar-keymap git-map :doc "Git map")
(keymap-set global-leader-map "j" git-map)


;; History
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq recentf-max-saved-items 50)

;; Backups & Versioning
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


;; Dictionary
(setq dictionary-server "dict.org")


;; Editing
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)


;; Text, Paragraph, and Sentences
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default fill-column 80)


;; Scrolling
(setq auto-window-vscroll nil)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)


;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)


;; Eshell & Shells
(setq eshell-scroll-to-bottom-on-output 'this)
(setq read-process-output-max (* 1024 1024))


;; Buffers
(setq global-auto-revert-non-file-buffers t)
(setq ibuffer-old-time 24)
(setq-default display-fill-column-indicator-column 100)
(setq-default display-line-numbers-type t)

;; Window Tiling
(setq-default frame-title-format '("%f"))
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
(setq max-mini-window-height 0.3)

;; Bells & Whistles
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)

;; Commands & Functions
(defun goto-configs ()
  "Go to emacs configs."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun project-directory ()
  "Current project directory."
  (let ((project (project-current)))
    (if project
        (project-root project))))

(defun ffap-project-match-1 (name)
  (let ((filename (match-string 1 name)))
    (if (project-current)
        (expand-file-name filename (project-directory))
      (expand-file-name filename default-directory))))

(defun treesit-install-languages ()
  "Install all language grammars registered with Treesitter"
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(defun compile-dwim ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

(defun comint ()
  (interactive)
  (universal-argument)
  (command-execute #'compile-dwim))

(defun relative-file-name ()
  "Relative from project or cwd directory."
  (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))

(defun absolute-file-name ()
  "Absolute path to file."
  (expand-file-name (buffer-file-name)))

(defun copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (relative-file-name)))

(defun copy-absolute-file-name ()
  "Copy absolute file path of current buffer."
  (interactive)
  (kill-new (absolute-file-name)))

(defun org-mode-setup ()
  (electric-indent-local-mode -1))
(add-hook 'org-mode-hook #'org-mode-setup)

(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)

(defun toggle-big-font ()
  "Toggle between the different font sizes in `toggle-big-font-sizes'."
  (interactive)
  (let ((current-size (pop toggle-big-font-sizes)))
    (add-to-list 'toggle-big-font-sizes current-size t))
  (set-face-attribute 'default nil :height (car toggle-big-font-sizes))
  (message "Font size set to %s" (car toggle-big-font-sizes)))
