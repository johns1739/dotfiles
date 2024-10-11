;; Global Keybindings
(keymap-global-set "<remap> <delete-horizontal-space>" #'cycle-spacing)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-#" #'dictionary-lookup-definition)
(keymap-global-set "M-L" #'duplicate-dwim)
(setq duplicate-line-final-position 1)


;; Global Leader Keybindings
(defvar-keymap global-leader-map :doc "Global leader keymap.")
(keymap-global-set "C-j" global-leader-map)
(keymap-global-set "M-j" global-leader-map)

;; Go To / Jump
(keymap-set global-leader-map "g" goto-map)
(bind-keys :map goto-map
           ("SPC" . switch-to-buffer)
           ("." . xref-find-definitions)
           ("," . xref-go-back)
           ("?" . xref-find-references)
           ("/" . xref-find-apropos)
           (":" . goto-line)
           (";" . scratch-buffer)
           ("'" . mode-line-other-buffer)
           ("%" . xref-find-references-and-replace)
           ("d" . dired-at-point)
           ("f" . find-file-at-point)
           ("h" . eldoc)
           ("j" . jump-to-register)
           ("J" . point-to-register)
           ("n" . next-error)
           ("o" . other-window)
           ("p" . previous-error)
           ("u" . goto-address-at-point))
(setq eldoc-echo-area-use-multiline-p nil)
(setq next-error-recenter '(4))
(setq next-error-highlight 1.0)
(setq next-error-highlight-no-select 1.0)
(setq next-error-message-highlight t)


;; Window Movement / Buffer management
(defvar-keymap window-movement-map :doc "Window movement map")
(keymap-set goto-map "w" window-movement-map)
(bind-keys :map window-movement-map
           ("SPC" . switch-to-buffer-other-window)
           ("f" . find-file-other-window)
           ("k" . kill-buffer)
           ("o" . other-window)
           ("p" . project-other-window-command)
           ("=" . balance-windows)
           ("." . xref-find-definitions-other-window)
           ("0" . delete-window)
           ("1" . delete-other-windows)
           ("2" . split-window-below-and-jump)
           ("3" . split-window-right-and-jump))
(defun split-window-below-and-jump ()
  "Split window below and jump to it."
  (interactive)
  (select-window (split-window-below)))
(defun split-window-right-and-jump ()
  "Split window right and jump to it."
  (interactive)
  (select-window (split-window-right)))


;; Tab Movement
(defvar-keymap tab-movement-map :doc "Tab movement map")
(keymap-set goto-map "t" tab-movement-map)
(bind-keys :map tab-movement-map
           (";" . tab-list)
           ("." . toggle-frame-tab-bar)
           ("," . tab-recent)
           ("/" . tab-switch)
           ("SPC" . switch-to-buffer-other-tab)
           ("0" . tab-close)
           ("1" . tab-close-other)
           ("2" . tab-new)
           ("f" . find-file-other-tab)
           ("p" . tab-previous)
           ("n" . tab-next)
           ("u" . tab-undo))


;; Search / Find
(keymap-set global-leader-map "s" search-map)
(bind-keys :map search-map
           ("." . rgrep)
           ("," . rgrep)
           ("/" . isearch-forward-thing-at-point)
           ("?" . occur)
           ("b" . bookmark-jump)
           ("d" . project-dired)
           ("f" . project-find-file)
           ("i" . imenu)
           ("s" . rgrep)
           ("r" . recentf-open))
(setq isearch-wrap-pause 'no)
(setq register-preview-delay 0.5)
(recentf-mode 1)


;; Project
(keymap-set global-leader-map "p" project-prefix-map)
(bind-keys :map project-prefix-map
           ("%" . project-query-replace-regexp)
           ("SPC" . project-switch-to-buffer))


;; Completion
(keymap-global-set "M-i" #'completion-at-point)
(keymap-global-set "M-I" #'hippie-expand)
(defvar-keymap completion-map :doc "Completion map")
(keymap-set global-leader-map "i" completion-map)
(fido-vertical-mode 1)
(setq completion-at-point-functions '(dabbrev-capf))
(setq completion-cycle-threshold 3)
(setq completions-detailed t)
(setq tab-always-indent t)
(setq completion-styles '(basic flex))
(setq completion-category-overrides '((file (styles . (partial-completion)))))
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq hippie-expand-verbose t)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                         try-expand-line
                                         try-expand-list
                                         try-expand-dabbrev
                                         try-expand-line-all-buffers
                                         try-expand-list-all-buffers
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name-partially
                                         try-complete-file-name))
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)


;; Toggle
(defvar-keymap toggle-map :doc "Toggle map")
(keymap-set global-leader-map "t" toggle-map)
(bind-keys :map toggle-map
           ("c" . load-theme)
           ("i" . display-fill-column-indicator-mode)
           ("I" . global-display-fill-column-indicator-mode)
           ("l" . display-line-numbers-mode)
           ("L" . global-display-line-numbers-mode))
(when (display-graphic-p)
  (bind-keys :map toggle-map
             ("f" . set-font-size))
  (defun set-font-size ()
    "Set the font size of Emacs"
    (interactive)
    (let ((font-size (min (max (read-number "Font size: " 12) 10) 24)))
      (set-face-attribute 'default nil :height (* 10 font-size))
      (message "Font size set to %s" font-size))))


;; Diagnostics
(defvar-keymap diagnostics-map :doc "Diagnostics map")
(keymap-set global-leader-map "k" diagnostics-map)
(bind-keys :map diagnostics-map
           (";" . flymake-show-buffer-diagnostics)
           ("P" . flymake-show-project-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error)
           :repeat-map diagnostics-repeat-map
           (";" . flymake-show-buffer-diagnostics)
           ("n" . flymake-goto-next-error)
           ("p" . flymake-goto-prev-error))
(setq flymake-fringe-indicator-position 'right-fringe)


;; Compilation
(defvar-keymap compilation-map :doc "Compilation map")
(keymap-set global-leader-map "c" compilation-map)
(bind-keys :map compilation-map
           ("." . compile-defun)
           ("," . eval-last-sexp)
           ("b" . eval-buffer)
           ("c" . compile-dwim)
           ("g" . recompile)
           ("i" . comint)
           ("p" . proced)
           ("r" . eval-region))
(setq-default proced-auto-update-flag t)
(setq next-error-find-buffer-function 'next-error-buffer-unnavigated-current)
(setq proced-auto-update-interval 1)
(setq proced-enable-color-flag t)
(setq compilation-window-height 20)
(setq compilation-context-lines 10)
(setq compilation-always-kill t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length 200)
(setq compilation-error-regexp-alist '())
(setq compilation-error-regexp-alist-alist '())
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]"
                                        1 ;; file
                                        2 ;; line
                                        nil ;; col
                                        nil ;; type
                                        1)) ;; hyperlink
  ;; TODO rspec
  ;;      # ./app/packages/flexwork/phone_numbers/spec/phone_numbers_spec.rb:15:in `block (4 levels) in <top (required)>'
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rails-test-target "^rails test \\([^:]+\\):\\([0-9]+\\)"
                                   1 ;; file
                                   2 ;; line
                                   nil ;; col
                                   nil ;; type
                                   1)) ;; hyperlink
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)"
                                      1 ;; file
                                      2 ;; line
                                      nil ;; col
                                      nil ;; type
                                      1)) ;; hyperlink
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rspec-backtrace-target "^ +# \\(./[A-Za-z0-9][^ (]*\\):\\([1-9][0-9]*\\)"
                    1 ;; file
                    2 ;; line
                    nil ;; col
                    nil ;; type
                    1)) ;; hyperlink
  (add-to-list 'compilation-error-regexp-alist 'rails-test-target)
  (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist 'rspec-backtrace-target))
(add-hook 'compilation-filter-hook  #'ansi-color-compilation-filter)


;; Notes
(defvar-keymap notes-map :doc "Notes map")
(keymap-set global-leader-map "n" notes-map)
(bind-keys :map notes-map
           (";" . org-agenda)
           ("." . org-capture-string)
           ("/" . org-search-view)
           ("?" . org-occur-in-agenda-files)
           ("," . org-capture-goto-last-stored)
           ("c" . org-capture)
           ("j" . org-capture-goto-target)
           ("l" . org-store-link)
           ("y" . copy-relative-file-name)
           ("Y" . copy-absolute-file-name))
(defvar notes-directory (locate-user-emacs-file "notes"))
(unless (file-exists-p notes-directory)
  (make-directory notes-directory))
(setq org-directory notes-directory)
(setq org-agenda-files `(,org-directory))
(setq org-return-follows-link nil)
(setq org-hide-emphasis-markers t)
(setq org-special-ctrl-a/e t)
(setq org-agenda-todo-ignore-deadlines 'far)
(setq org-agenda-sorting-strategy
      '(priority-down
        time-up
        habit-up
        deadline-up
        scheduled-up
        category-keep
        todo-state-down
        effort-down
        tag-up
        timestamp-up
        ts-up
        tsia-up
        alpha-up))
(setq org-tag-faces '(("bug"  . "sienna")
                      ("feature" . "goldenrod")
                      ("ticket" . "khaki")))
(setq org-capture-templates `(("t" "Work Task"
                               entry (file+headline ,(locate-user-emacs-file "notes/tasks.org") "Tasks")
                               "* TODO %? %^g\n%t\n%i"
                               :prepend t
                               :empty-lines 1)
                              ("n" "Work Note"
                               entry (file+olp ,(locate-user-emacs-file "notes/tasks.org") "Notes")
                               "* %? \n%i"
                               :prepend t
                               :empty-lines 1)
                              ("j" "Personal Journal"
                               entry (file+olp+datetree ,(locate-user-emacs-file "notes/personal.org") "Journal")
                               "* %?\n%U")
                              ("p" "Personal Task"
                               entry (file+olp,(locate-user-emacs-file "notes/personal.org") "Tasks")
                               "* %? \n%i"
                               :prepend t
                               :empty-lines 1)))
(setq org-todo-keyword-faces '(("TODO" . "goldenrod")
                               ("BUILDING" . "khaki")
                               ("PULLREQUEST" . "forest green")
                               ("DONE" . "dark olive green")
                               ("CANCELED" . "sienna")))
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t))))


;; Git
(defvar-keymap git-map :doc "Git map")
(keymap-set global-leader-map "j" git-map)


;; Hooks
(add-hook 'before-save-hook #'whitespace-cleanup)
;; (with-eval-after-load 'ispell
;;   (when (executable-find ispell-program-name)
;;     (add-hook 'text-mode-hook #'flyspell-mode)
;;     (add-hook 'prog-mode-hook #'flyspell-prog-mode)))
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'hl-line-mode)


;; Modes
(auto-save-visited-mode -1) ;; Annoying with whitespace cleanup constantly moving the point
(column-number-mode -1)
(delete-selection-mode -1)
(desktop-save-mode -1) ;; After a while, CPU gets bogged down with all tracked files under LSP
(electric-indent-mode t)
(electric-pair-mode -1)
(global-auto-revert-mode t)
(global-eldoc-mode t)
(global-so-long-mode t)
(line-number-mode t)
(pixel-scroll-precision-mode t)
(repeat-mode nil) ;; Sometimes gets in the way.
(save-place-mode t)
(savehist-mode t)
(window-divider-mode -1)
(setq-default indent-tabs-mode nil)


;; Display Buffer
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


;; Settings
(setq global-auto-revert-non-file-buffers t)
(setq history-delete-duplicates t)
(setq history-length 1000)
(setq ibuffer-old-time 24)
(setq kill-do-not-save-duplicates t)
(setq recentf-auto-cleanup 300)
(setq recentf-max-saved-items 50)
(setq require-final-newline t)
(setq show-paren-context-when-offscreen 'show-paren-context-when-offscreen)
(setq dictionary-server "dict.org")
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 120)
(setq-default display-line-numbers-type t)
(setq-default tab-width 4)
(setq eshell-scroll-to-bottom-on-output 'this)
(setq read-process-output-max (* 1024 1024))


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


;; Scrolling
(setq auto-window-vscroll nil)
(setq scroll-margin 3)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)


;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)


;; Emacs Bells & Whistles
(setq-default frame-title-format '("%f"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(setq-default cursor-type 'bar)


;; Mouse
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))


;; FFAP find-file-at-point
(defun ffap-project-match-1 (name)
  (let ((filename (match-string 1 name)))
    (if (and (project-current) (not (string-prefix-p "./" filename)))
        (expand-file-name filename (project-directory))
      (expand-file-name filename default-directory))))
(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1)))


;; Commands & Functions
(defun project-directory ()
  "Current project directory."
  (let ((project (project-current)))
    (if project
        (project-root project))))

(defun compile-dwim ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun comint ()
  (interactive)
  (universal-argument)
  (command-execute #'compile-dwim))

(defun current-directory ()
  "Current project directory or cwd."
  (or (project-directory) default-directory))

(defun current-directory-base ()
  (f-base (current-directory)))

(defun relative-file-name ()
  "Relative from project or cwd directory."
  (file-relative-name (buffer-file-name) (current-directory)))

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


;; Eglot
(defun eglot-set-bindings ()
  "Inject eglot bindings."
  (bind-keys :map (current-local-map)
             ([remap indent-buffer] . eglot-format)))
(add-hook 'eglot-managed-mode-hook #'eglot-set-bindings)


;; Treesitter
(defun treesit-install-default-languages ()
  "Install all language grammars registered with Treesitter"
  (interactive)
  (require 'treesit)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (gleam "https://github.com/gleam-lang/tree-sitter-gleam/")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
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
          (scheme "https://github.com/6cdh/tree-sitter-scheme"))))


;; Mode-Line
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:eval (mode-line-project-name-format))
                (:eval (mode-line-modified-format))
                (:eval (mode-line-buffer-name-format))
                "%n   %o  L%l%n%[%]  "
                (:eval (flymake-mode-line-format))
                " "
                "%[" (:eval (mode-line-major-mode-format)) "%] "
                mode-line-end-spaces))

(defun flymake-mode-line-format ()
  "Display flymake diagnostics in the mode line."
  (if (bound-and-true-p flymake-mode)
      '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(defun mode-line-project-name-format ()
  "Display project name in mode line."
  (if (project-current)
      (propertize (project-name (project-current)) 'face 'bold)
    ""))

(defun mode-line-buffer-name-format ()
  "Display buffer name in mode line."
  (let ((face (if (or (not (buffer-file-name))
                      (buffer-modified-p)
                      (not (verify-visited-file-modtime)))
                  'italic
                nil)))
    (propertize (mode-line-buffer-name) 'face face)))

(defvar mode-line-buffer-name-size 60
  "Max size of the buffer-name in the mode line.")

(defvar mode-line-buffer-name-squish-method #'squish-path-truncate-left
  "Squish method to use when mode-line-buffer-name overflows.")

(defun mode-line-buffer-name ()
  (if (buffer-file-name)
      (funcall mode-line-buffer-name-squish-method (relative-file-name) mode-line-buffer-name-size)
    (buffer-name)))

(defun squish-path-truncate-left (path max-length)
  "Squish path by truncating the left to max length."
  (if (> (length path) max-length)
      (string-truncate-left path max-length)
    path))

(defun squish-path-to-initials (path max-length)
  "Squish path to max length by replacing folder names with initials."
  (if (> (length path) max-length)
      ;; TODO: f-split sometimes not autoloaded ???
      (let* ((parts (file-name-split path))
             (name (car (last parts)))
             (overflow (- (length path) max-length))
             (squished-parts
              (mapcar (lambda (part)
                        (if (<= overflow 0)
                            part
                          (progn
                            (setq overflow (- overflow (- (length part) 1)))
                            (substring part 0 1))))
                      (butlast parts)))
             (new-path (string-join (append squished-parts (list name)) "/")))
        (if (> (length new-path) max-length)
            name
          new-path))
    path))

(defun mode-line-modified-format ()
  "Display modified status in mode line."
  (if (and (buffer-file-name) (buffer-modified-p))
      " * "
    " "))

(defun mode-line-major-mode-format ()
  "Display major mode in mode line."
  (string-join
   (string-split
    (capitalize
     (string-remove-suffix
      "-ts"
      (string-remove-suffix "-mode" (symbol-name major-mode))))
    "-")
   ""))
