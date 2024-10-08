;; Global Leader Keybindings
(defvar-keymap global-leader-map :doc "Global leader keymap.")
(keymap-global-set "C-j" global-leader-map)


;; Project
(keymap-set global-leader-map "p" project-prefix-map)


;; Go To
(keymap-set global-leader-map "g" goto-map)


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

;; Search
(keymap-set global-leader-map "s" search-map)
(bind-keys :map search-map
           ("n" . org-search-view)
           ("N" . org-occur-in-agenda-files)
           ("t" . load-theme))
(setq xref-search-program 'ripgrep)


;; Completion
(defvar-keymap completion-map :doc "Completion map")
(keymap-set global-leader-map "i" completion-map)
(bind-keys :map completion-map
           ("," . dabbrev-completion)
           ("." . completion-at-point)
           ("/" . hippie-expand))
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)


;; Toggle
(defvar-keymap toggle-map :doc "Toggle map")
(keymap-set global-leader-map "t" toggle-map)
(bind-keys :map toggle-map
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


;; Git
(defvar-keymap git-map :doc "Git map")
(keymap-set global-leader-map "j" git-map)


;; Diagnostics
(keymap-set global-leader-map "k" diagnostics-map)


;; Compilation
(defvar-keymap compilation-map :doc "Compilation map")
(keymap-set global-leader-map "c" compilation-map)
(bind-keys :map compilation-map
           ("!" . project-async-shell-command)
           ("." . compile-defun)
           ("," . compilation-goto-in-progress-buffer)
           ("b" . eval-buffer)
           ("c" . compile-dwim)
           ("e" . eval-last-sexp)
           ("E" . eval-print-last-sexp)
           ("g" . recompile)
           ("i" . comint)
           ("p" . proced)
           ("r" . eval-region))
(setq-default proced-auto-update-flag t)
(setq next-error-find-buffer-function 'next-error-buffer-unnavigated-current)
(setq proced-auto-update-interval 1)
(setq proced-enable-color-flag t)
(setq compilation-window-height 20)
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


;; Buffer Maintenance
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
(electric-indent-mode +1)
(electric-pair-mode -1)
(global-eldoc-mode +1)
(global-so-long-mode t)
(line-number-mode +1)
(repeat-mode -1) ;; Sometimes gets in the way.
(window-divider-mode -1)
(pixel-scroll-precision-mode 1)


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


;; find-file-at-point
(defun ffap-project-match-1 (name)
  (let ((filename (match-string 1 name)))
    (if (and (project-current) (not (string-prefix-p "./" filename)))
        (expand-file-name filename (project-directory))
      (expand-file-name filename default-directory))))
(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\([^\s]+\\):?" . ffap-project-match-1)))


;; Dictionary
(setq dictionary-server "dict.org")


;; Text, Paragraph, and Sentences
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 120)
(setq-default display-line-numbers-type t)


;; Eshell & Shells
(setq eshell-scroll-to-bottom-on-output 'this)
(setq read-process-output-max (* 1024 1024))


;; Extra Commands & Functions
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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((ruby-mode ruby-ts-mode)
                 . ("solargraph" "stdio" :initializationOptions
                    (;; options
                     :useBundler t
                     :diagnostics t
                     :completion t
                     :hover t
                     :autoformat :json-false
                     :formatting t
                     :symbols t
                     :definitions t
                     :rename t
                     :references t
                     :folding t)))))

;; Treesitter
(defun treesit-install-default-languages ()
  "Install all language grammars registered with Treesitter"
  (interactive)
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
