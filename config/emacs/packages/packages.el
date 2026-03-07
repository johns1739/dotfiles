;;; -*- lexical-binding: t -*-

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			  ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 100) ("melpa-stable" . 75)))
(setq use-package-always-ensure t)
(package-initialize)

;; Install early for downstream dependencies
(use-package exec-path-from-shell
  :demand
  :if (and (memq window-system '(mac ns x)) (display-graphic-p))
  :custom
  (exec-path-from-shell-debug t)
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package emacs ;; graphics only
  :demand
  :if (display-graphic-p)
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode)
  :init
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 120))
  :custom
  (prettify-symbols-alist '(("!=" . ?≠)
                            ;; ("&&" . ?∧)
                            ("->" . ?→)
                            ("->>" . ?↠)
                            ("<-" . ?←)
                            ("<<" . ?«)
                            ("<=" . ?≤)
                            ("<>" . ?◇)
                            ("<|" . ?◁)
                            ;; ("==" . ?≡)
                            ("=>" . ?⇒)
                            (">=" . ?≥)
                            (">>" . ?»)
                            ;; ("not" . ?¬)
                            ("|>" . ?▷)
                            ;; ("||" . ?∨)
                            )))

(use-package emacs ;; terminal only
  :demand
  :unless (display-graphic-p)
  :config
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(use-package emacs
  :demand
  :init
  (defvar-keymap global-leader-map :doc "Global leader keymap.")
  (keymap-set ctl-x-map "SPC" global-leader-map)
  (keymap-set global-leader-map "g" goto-map)
  (keymap-set global-leader-map "s" search-map)
  :bind ( :map global-map
          ([remap backward-sentence] . backward-sexp)
          ([remap forward-sentence] . forward-sexp)
          ([remap split-window-below] . split-window-below-and-jump)
          ([remap split-window-right] . split-window-right-and-jump)
          ([remap downcase-word] . downcase-dwim)
          ([remap upcase-word] . upcase-dwim)
          ("C-M-;" . comment-indent)
          ("C-j" . comment-indent-new-line)
          ("C-x K" . kill-this-buffer)
          ("M-I" . completion-at-point)
          ("M-j" . join-line)
          ("M-L" . duplicate-dwim)
          ("M-n" . forward-paragraph)
          ("M-o" . other-window)
          ("M-p" . backward-paragraph)
          :map global-leader-map
          ("TAB" . indent-format-buffer)
          ("SPC" . project-switch-to-buffer)
          ("=" . balance-windows-area)
          ("0" . delete-window)
          ("1" . delete-other-windows)
          ("2" . split-window-below-and-jump)
          ("3" . split-window-right-and-jump)
          ;; Copy/Paste/Edits
          ("x l" . keep-lines)
          ("x k" . delete-matching-lines)
          ("x u" . delete-duplicate-lines)
          ("x s" . sort-lines)
          ("x w" . whitespace-cleanup)
          ;; Compilation & Computation
          ("k *" . calc)
          ("k r" . ielm)
          ;; Settings (Look & Feel)
          (", ," . open-custom-file)
          (", <" . open-packages-dired)
          (", +" . global-text-scale-adjust)
          (", =" . balance-windows-area)
          (", D" . toggle-debug-on-error)
          (", F" . toggle-frame-fullscreen)
          (", R" . restart-emacs)
          (", SPC" . load-theme)
          (", c" . display-fill-column-indicator-mode)
          (", f" . toggle-frame-maximized)
          (", h" . hl-line-mode)
          (", n" . display-line-numbers-mode)
          (", r" . reload-emacs)
          (", t" . toggle-truncate-lines)
          (", x" . describe-font)
          :map mode-specific-map
          ("C-o" . goto-address-at-point)
          :map goto-map
          ("SPC" . switch-to-buffer)
          ("." . xref-find-definitions)
          (">" . eldoc)
          ("," . xref-go-back)
          (";" . scratch-buffer)
          (":" . goto-line)
          ("?" . xref-find-references)
          ("/" . xref-find-apropos)
          ("'" . mode-line-other-buffer)
          ("f" . find-file-at-point)
          ("d" . dired-jump)
          ("i" . imenu)
          ("j" . jump-to-register)
          ("J" . point-to-register)
          ("l" . goto-line)
          ("m" . bookmark-jump)
          ("M" . bookmark-set)
          ;; Window navigation
          ("w h" . windmove-left)
          ("w j" . windmove-down)
          ("w k" . windmove-up)
          ("w l" . windmove-right)
          ("w H" . windmove-swap-states-left)
          ("w J" . windmove-swap-states-down)
          ("w K" . windmove-swap-states-up)
          ("w L" . windmove-swap-states-right)
          :map search-map
          ("g" . rgrep)
          ("j" . list-registers)
          ("m" . list-bookmarks)
          ("o" . occur)
          ("r" . recentf-open))
  :custom
  (auto-revert-avoid-polling t)
  (auto-window-vscroll nil)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (basic partial-completion)))))
  (completion-cycle-threshold 3)
  (completion-ignore-case t)
  (completion-styles '(basic substring partial-completion))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height 20)
  (confirm-kill-emacs 'y-or-n-p)
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (display-time-default-load-average nil)
  (duplicate-line-final-position 1)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (enable-recursive-minibuffers t) ;; Might be confusing
  (fast-but-imprecise-scrolling t)
  (find-file-visit-truename t)
  (global-auto-revert-non-file-buffers t)
  (history-delete-duplicates t)
  (history-length 1000)
  (imenu-max-item-length 80)
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (initial-scratch-message nil)
  (isearch-wrap-pause 'no)
  (kill-do-not-save-duplicates t)
  (max-mini-window-height 0.2)
  (next-error-find-buffer-function 'next-error-buffer-unnavigated-current)
  (next-error-highlight 1.0)
  (next-error-highlight-no-select 1.0)
  (next-error-message-highlight t)
  (next-error-recenter '(4))
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 100)
  (register-preview-delay 0.5)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (scroll-conservatively most-positive-fixnum)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (show-paren-context-when-offscreen 'show-paren-context-when-offscreen)
  (tab-always-indent t)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-handled-backends '(Git))
  :hook
  (compilation-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (special-mode . hl-line-mode)
  (before-save . nuke-trailing-whitespace)
  :config
  (setq-default cursor-type 'bar)
  (setq-default display-fill-column-indicator-column 100)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil) ;; use spaces instead of tabs
  (setq-default tab-width 4)
  (auto-save-visited-mode -1) ;; auto-format constantly triggers, annoying
  (column-number-mode -1)
  (delete-selection-mode -1)
  (desktop-save-mode -1) ;; CPU heavy when loading many buffers under LSP
  (electric-indent-mode t)
  (electric-pair-mode -1)
  (global-auto-revert-mode t)
  (global-eldoc-mode t)
  (global-so-long-mode t)
  (line-number-mode t)
  (pixel-scroll-precision-mode t)
  (recentf-mode 1)
  (repeat-mode -1) ;; Sometimes gets in the way.
  (save-place-mode t)
  (savehist-mode t)
  (window-divider-mode (display-graphic-p))
  (defun open-init-file ()
    (interactive)
    (find-file user-init-file))
  (defun open-packages-dired ()
    (interactive)
    (dired (locate-user-emacs-file "packages/")))
  (defun open-custom-file ()
    (interactive)
    (if (boundp 'custom-file)
        (find-file custom-file)))
  (defun split-window-below-and-jump ()
    "Split window below and jump to it."
    (interactive)
    (select-window (split-window-below)))
  (defun split-window-right-and-jump ()
    "Split window right and jump to it."
    (interactive)
    (select-window (split-window-right)))
  (defun indent-format-buffer ()
    (interactive)
    (save-excursion
      (whitespace-cleanup)
      (indent-region (point-min) (point-max) nil)))
  (defun reload-emacs ()
    (interactive)
    (load (locate-user-emacs-file "init.el") :no-error-if-file-is-missing))
  (defun nuke-trailing-whitespace ()
    ;; Running delete-trailing-whitespace on certain special modes can cause issues.
    ;; So only run in prog-mode.
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace))))

(use-package custom
  :bind (:map global-leader-map
              ("x y" . copy-relative-file-name)
              ("x Y" . copy-absolute-file-name))
  :init
  (defun absolute-file-name ()
    "Absolute path to file."
    (expand-file-name (buffer-file-name)))
  (defun copy-absolute-file-name ()
    "Copy absolute file path of current buffer."
    (interactive)
    (let ((afn (absolute-file-name)))
      (kill-new (absolute-file-name))
      (message "Copied %s" afn)))
  (defun project-directory ()
    "Current project directory."
    (let ((project (project-current)))
      (and project (project-root project))))
  (defun relative-file-name ()
    "Relative from project or cwd directory."
    (file-relative-name (buffer-file-name) (or (project-directory) default-directory)))
  (defun copy-relative-file-name ()
    "Copy file path of current buffer relative to project directory."
    (interactive)
    (let ((rfn (relative-file-name)))
      (kill-new (relative-file-name))
      (message "Copied %s" rfn))))

(use-package ace-window
  :if (display-graphic-p)
  :bind  (([remap other-window] . ace-window)
          :map goto-map
          ("w 0" . ace-delete-window)
          ("w 1" . ace-delete-other-windows)
          ("w o" . ace-select-window)
          ("w O" . ace-swap-window)))

(use-package agent-shell
  ;; https://github.com/xenodium/agent-shell
  :if (display-graphic-p)
  :commands (agent-shell)
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(agent-shell "Agent" "I")))
  :bind ( :map global-leader-map
          ("I" . agent-shell))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.5))))

(use-package aidermacs
  :disabled ;; too expensive, requires python dependency aider
  :if (and (display-graphic-p) (executable-find "aider"))
  :bind ( :map global-leader-map
          ("a" . aidermacs-transient-menu))
  :custom
  ;; (aidermacs-default-model "gpt-5.2")
  ;; (aidermacs-default-model "gemini-2.5-pro"))
  (aidermacs-default-chat-mode 'architect))

(use-package auto-dark ;; auto switching dark / light color themes
  :disabled ;; better to manually select
  ;; (setopt auto-dark-themes '((wombat) (leuven)))
  ;; (auto-dark-mode)
  :commands (auto-dark-mode))

(use-package avy
  :bind (([remap goto-line] . avy-goto-line)
         :map global-leader-map
         ("n w" . avy-org-refile-as-child)
         ("x g p" . avy-copy-line)
         ("x g P" . avy-copy-region)
         ("x g g" . avy-move-line)
         ("x g G" . avy-move-region)
         ("x g k" . avy-kill-whole-line)
         ("x g K" . avy-kill-region)
         ("x g y" . avy-kill-ring-save-whole-line)
         ("x g Y" . avy-kill-ring-save-region)
         :map isearch-mode-map
         ("M-g" . avy-isearch)
         :map goto-map
         ("G" . avy-resume)
         ("g" . avy-goto-char-timer)))

(use-package beacon
  :if (display-graphic-p) ;; Not pretty in terminal
  :config
  (beacon-mode 1))

(use-package calc
  :bind ( :map calc-mode-map
          ("i" . nil)))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :custom
  ;; #'cape-dict ;; no need for dicts
  ;; #'cape-elisp-symbol ;; elisp buffers already set its own cape func.
  ;; #'cape-line ;; Kinda buggy
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-file
         #'cape-elisp-block)))

(use-package casual ;; Better transient menu
  :disabled ;; Too much configuration for different modes.
  :bind ( :map org-agenda-mode-map
          ("C-o" . casual-agenda-tmenu)))

(use-package command-log-mode
  :disabled ;; Use C-h l
  :bind (:map global-leader-map
              ("m l" . clm/toggle-command-log-buffer))
  :config
  (global-command-log-mode))

(use-package compile
  :bind (:map global-leader-map
              ("k g" . recompile)
              ("k k" . compile-dwim)
              ("k K" . compile)
              ("k n" . next-error)
              ("k p" . previous-error)
              ("k RET" . send-region-to-process))
  :custom
  (compilation-always-kill t)
  (compilation-context-lines 10)
  (compilation-error-regexp-alist '())
  (compilation-error-regexp-alist-alist '())
  (compilation-max-output-line-length 121)
  (compilation-scroll-output 'first-error)
  (compilation-search-path '(nil)) ;; directories to search for files
  (compilation-skip-threshold 2) ;; skip warnings and info with next-error
  (compilation-window-height 20)
  (compile-command "make ")
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :init
  (defun compile-dwim ()
    (interactive)
    (if (project-current)
        (call-interactively #'project-compile)
      (call-interactively #'compile)))
  (defun send-region-to-process (arg beg end)
    """
    Send the current region to a process buffer.
    The first time it's called, will prompt for the buffer to
    send to. Subsequent calls send to the same buffer, unless a
    prefix argument is used (C-u), or the buffer no longer has an
    active process.
    """
    (interactive "P\nr")
    (if (or arg ;; user asks for selection
            (not (boundp 'send-region-to-process-target)) ;; target not set
            ;; or target is not set to an active process:
            (not (process-live-p (get-buffer-process
                                  send-region-to-process-target))))
        (setq send-region-to-process-target
              (completing-read
               "Process: "
               (seq-map (lambda (el) (buffer-name (process-buffer el)))
                        (process-list)))))
    (process-send-region send-region-to-process-target beg end))
  :config
  ;; Make dir local variables work in compilation buffers
  (add-hook 'compilation-mode-hook 'hack-dir-local-variables-non-file-buffer)
  ;; options: file-group-num, line-group-num, col-group-num, type, hyperlink
  ;; (add-to-list 'compilation-error-regexp-alist 'failure-newline-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(failure-newline-target
                 "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]" 1 2 nil nil 1))
  ;; (add-to-list 'compilation-error-regexp-alist 'simple-spaced-target)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(simple-spaced-target
                 "^ +\\([A-Za-z0-9/][^ (]*\\):\\([1-9][0-9]*\\)" 1 2 nil nil 1)))

(use-package consult
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer" "SPC"))
    (add-to-list 'project-switch-commands '(consult-ripgrep "Search" "s")))
  :bind (([remap Info-search] . consult-info)
         ([remap bookmark-jump] . consult-bookmark)
         ;; ([remap goto-line] . consult-goto-line) ;; prefer avy-goto-line
         ([remap imenu] . consult-imenu)
         ([remap keep-lines] . consult-keep-lines)
         ([remap isearch-edit-string] . consult-isearch-history)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap load-theme] . consult-theme)
         ([remap recentf-open] . consult-recent-file)
         ([remap org-search-view] . consult-org-agenda)
         ([remap list-registers] . consult-register)
         ([remap jump-to-register] . consult-register-load)
         ([remap point-to-register] . consult-register-store)
         ([remap keep-lines] . consult-keep-lines)
         ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
         :map global-leader-map
         ("k SPC" . consult-flymake)
         ("n /" . consult-org-heading)
         :map minibuffer-mode-map
         ("M-i" . consult-history)
         :map search-map
         ("f" . consult-find) ;; works even if not in a project
         ("l" . consult-line)
         ("L" . consult-focus-lines)
         ("s" . consult-ripgrep)
         :map goto-map
         ("I" . consult-imenu-multi)
         ("h" . consult-outline))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (if (executable-find "fd")
      (bind-keys :map search-map
                 ("f" . consult-fd))))

(use-package consult-flycheck
  :disabled ;; not using flycheck
  :after (consult flycheck)
  :commands (consult-flycheck))

(use-package consult-denote ;; Prot's note-taking with org
  :disabled ;; not using denote
  :bind (:map global-leader-map
              ("n d f" . consult-denote-find)
              ("n d s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package consult-eglot
  :after (eglot consult)
  :bind (:map global-leader-map
              ("l i" . consult-eglot-symbols)))

(use-package copilot
  ;; M-x copilot-install-server
  ;; M-x copilot-login
  :if (executable-find "npm")
  :commands (copilot-mode)
  :bind ( :map global-leader-map
          ("i c" . copilot-mode)
          :map copilot-completion-map
          ("M-f" . copilot-accept-completion-by-word)
          ("M-e" . copilot-accept-completion-by-line)
          ("M-<return>" . copilot-accept-completion))
  :custom
  (copilot-idle-delay 0.5)
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t)
  :custom-face
  (copilot-overlay-face
   ((t ( :family "JetBrainsMonoNL Nerd Font Mono"
         :slant italic
         :weight ultra-light
         :inherit completions-annotations)))))

(use-package copilot-chat
  :disabled ;; too slow, better to use gptel
  :if (display-graphic-p)
  :requires copilot
  :after (request org markdown-mode copilot))

(use-package corfu
  :demand
  :if (display-graphic-p)
  ;; When corfu-auto is off, better to not modify bindings.
  :bind ( :map corfu-map
          ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-separator ?\s)
  (corfu-echo-delay 0.2)
  (corfu-popupinfo-delay '(2.0 . 0.5))
  (corfu-min-width 20)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package dashboard
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :bind ( :map search-map
          ("g" . deadgrep)
          ("G" . rgrep)
          :map deadgrep-mode-map
          ("C-w" . deadgrep-edit-mode)))

(use-package denote
  :disabled ;; prefer org-mode note taking
  :bind (:map global-leader-map
              ("n d SPC" . denote-open-or-create)
              ("n d n" . denote)
              ("n d j" . denote-journal-extras-new-or-existing-entry)
              ("n d l" . denote-link-or-create)
              ("n d k" . denote-find-link)
              ("n d K" . denote-find-backlink)
              ("n d r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory "~/workspaces/notes")
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode))

(use-package devdocs
  :disabled ;; clunky and difficult to keep updated.
  :bind (:map global-leader-map
              ("m h I" . devdocs-install)
              ("m h h" . devdocs-lookup)
              ("m h s" . devdocs-search)))

(use-package diff-hl ;; git diff changes in fringe
  ;; not really used, better to use magit-diff. Conflicts w/ meow bindings.
  ;; :bind (:map global-leader-map
  ;;             ("m d" . diff-hl-show-hunk))
  :after magit
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package diff-mode
  :commands (diff diff-mode)
  :bind ( :map diff-mode-map
          ("M-o" . nil)))

(use-package dimmer
  :disabled ;; TODO: Figure out error issue with latest version
  :if (display-graphic-p) ;; Only works in GUI
  :config
  (dimmer-mode t))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :hook
  (dired-mode . hl-line-mode))

(use-package dired-subtree
  ;; TODO
  :disabled ;; Not really used. How to install informal repo?
  :init
  (with-eval-after-load 'dired-mode
    (require 'dired-subtree))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package docker
  :disabled ;; rarely used
  :if (and (display-graphic-p) (executable-find "docker"))
  :bind (:map global-leader-map
              ("k o" . docker))
  :config
  (let ((column (seq-find (lambda (col) (equal (plist-get col :name) "Image"))
                          docker-container-columns)))
    (plist-put column :width 62)))

(use-package eat
  ;; When eat-terminal input is acting weird, try re-compiling with command:
  ;; (eat-compile-terminfo)
  :if (and (display-graphic-p)
           (not (string-suffix-p "/bin/fish" (getenv "SHELL"))))
  :bind ( :map global-leader-map
          ("k t" . eat-project)
          ("k T" . eat)
          :map eat-semi-char-mode-map
          ("M-o" . other-window))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(eat-project "Eat" "t")))
  :custom
  (eat-enable-auto-line-mode nil) ;; more intuitive to use semi-char mode
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (process-adaptive-read-buffering t)
  :hook
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-load . eat-eshell-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eat\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.3))))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :bind ( :map global-leader-map
          ("x d" . ediff-files)))

(use-package eglot
  :bind ( :map global-leader-map
          ("L" . eglot)
          ("l TAB" . eglot-format)
          ("l e" . eglot-events-buffer)
          ("l E" . eglot-stderr-buffer)
          ("l l" . eglot-reconnect)
          ("l q" . eglot-shutdown)
          ("l Q" . eglot-shutdown-all)
          ("l r" . eglot-rename)
          ("l d" . eglot-find-declaration)
          ("l a" . eglot-code-actions))
  :config
  (setq eglot-mode-line-session nil))

(use-package eldoc-box
  :disabled ;; annoying GUI
  :if (display-graphic-p)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

(use-package elfeed
  :commands (elfeed)
  :bind ( :map global-leader-map
          ("m f" . elfeed))
  :custom
  (elfeed-feeds
   '(("http://nullprogram.com/feed/" null emacs)
     ("https://planet.emacslife.com/atom.xml" emacslife emacs)
     ("https://modern-sql.com/feed" modernsql sql)
     ;; ("https://www.reddit.com/r/ExperiencedDevs/top/.rss?t=month" reddit news)
     ;; ("https://hnrss.org/frontpage" hn news)
     ;; ("https://hnrss.org/jobs" hn jobs)
     ("https://lobste.rs/rss" lobste news))))

(use-package ellama
  :disabled ;; prefer gptel
  :custom
  (ellama-user-nick "Lobo")
  (ellama-assistant-nick "Cody")
  (ellama-language "English")
  (ellama-spinner-enabled t)
  ;; (ellama-chat-display-action-function #'display-buffer-full-frame)
  ;; (ellama-instant-display-action-function #'display-buffer-at-bottom)
  (ellama-keymap-prefix "C-;")
  (ellama-auto-scroll t)
  :hook
  (org-ctrl-c-ctrl-c . ellama-chat-send-last-message)
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama :chat-model "qwen2.5:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama :chat-model "qwen2.5-coder:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-summarization-provider
          (make-llm-ollama :chat-model "qwen2.5-coder:7b"
                           :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (ellama-context-header-line-global-mode 1))

(use-package elysium
  :disabled ;; doesn't work very well, buggy.
  :after (gptel)
  :custom
  (elysium-window-size 0.5)
  (elysium-window-style 'vertical)
  :bind (:map global-leader-map
              ("i ." . elysium-query)
              ("i >" . elysium-add-context)
              ("i ," . elysium-toggle-window)
              ("i <" . elysium-clear-buffer))
  :hook
  (elysium-apply-changes . smerge-mode))

(use-package envrc
  ;; Must activate at the end
  :hook (after-init . envrc-global-mode))

(use-package eshell
  :bind ( :map global-leader-map
          ("k e" . project-eshell)
          ("k E" . eshell))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*eshell\\*" (display-buffer-in-side-window)
                 (window-height . 0.3))))

(use-package ffap
  :commands (find-file-at-point)
  :init
  (defun ffap-deep-match-file (filename)
    (let ((project-dir (project-directory)))
      (or (and project-dir (ffap-deep-match-file-string filename project-dir))
          (ffap-deep-match-file-string filename default-directory))))
  (defun ffap-deep-match-file-string (filename dir)
    (let* ((deep-1 (file-name-concat "**" filename))
           (deep-2 (file-name-concat "**" "**" filename))
           (files  (or (file-expand-wildcards (expand-file-name deep-1 dir) t)
                       (file-expand-wildcards (expand-file-name deep-2 dir) t))))
      (and files (car files))))
  :config
  (add-to-list 'ffap-alist '("" . ffap-deep-match-file)))

(use-package files ;; backups
  :custom
  (backup-by-copying t)
  (backup-directory-alist `(("." . "~/.backups")))
  (create-lockfiles nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 3)
  (make-backup-files t)
  (vc-make-backup-files t)
  (version-control t)
  :config
  (defun buffer-backed-up-set-to-time ()
    "Set the buffer-backed-up variable to the current time if t."
    (if (eq buffer-backed-up t)
        (setq buffer-backed-up (current-time))))
  (defun buffer-backed-up-reset-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (buffer-backed-up-set-to-time)
    (if (and buffer-backed-up
             (time-less-p (time-add (buffer-backed-up) (* 60 60 24)) ;; 24hrs
                          (current-time)))
        (setq buffer-backed-up nil))
    (let ((orig-fun-result (apply orig-fun args)))
      (buffer-backed-up-set-to-time)
      orig-fun-result))
  (advice-add 'backup-buffer :around #'buffer-backed-up-reset-advice))

(use-package flycheck
  :disabled ;; prefer flymake
  ;; only used with lsp-mode
  ;; https://www.flycheck.org/en/latest/
  :commands (global-flycheck-mode flycheck-mode)
  :init
  (defun flycheck-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap consult-flymake] . consult-flycheck)
               ([remap flymake-show-buffer-diagnostics] . consult-flycheck)
               ([remap flymake-show-diagnostic] . flycheck-display-error-at-point)
               ([remap flymake-show-project-diagnostics] . nil)
               ([remap flymake-goto-next-error] . flycheck-next-error)
               ([remap flymake-goto-prev-error] . flycheck-previous-error)))
  :custom
  (flycheck-indication-mode 'right-fringe)
  :hook
  (flycheck-mode . flycheck-set-bindings))

(use-package flymake
  :bind (:map global-leader-map
              ("k d" . flymake-show-buffer-diagnostics)
              ("k D" . flymake-show-project-diagnostics))
  :custom
  (flymake-fringe-indicator-position 'left-fringe))

(use-package flyspell
  ;; brew install aspell
  ;; brew install ispell
  ;; ispell fails to install due to compilation issues
  :if (or (executable-find "aspell") (executable-find "ispell"))
  :config
  (if (executable-find "ispell")
      (setq ispell-program-name "ispell")
    (setq ispell-program-name "aspell")))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eglot-booster
  ;; cargo install emacs-lsp-booster
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         :map ctl-x-map
         ("A" . embark-act)
         ("C" . embark-collect)
         ("E" . embark-export)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package find-file-in-project
  :bind (:map goto-map
              ("f" . find-file-in-project-at-point)
              ("F" . find-file-in-project-by-selected)))

(use-package forge
  ;; setup:
  ;; Create ~/.authinfo with content:
  ;; machine api.github.com login USERNAME^forge password TOKEN
  ;; where USERNAME: git config --global github.user jubajr17
  ;; and TOKEN: from https://github.com/settings/tokens
  ;;            in a browser to generate a new "classic" token using
  ;;            the repo, user and read:org scopes
  ;; Run M-x auth-source-forget-all-cached
  :commands (forge-dispatch)
  :custom
  ;; password authentication service
  ;; To reload authinfo:
  ;; (auth-source-forget-all-cached)
  (auth-sources '("~/.authinfo")))

(use-package git-link
  :bind (:map global-leader-map
              ("x j" . git-link)
              ("x J" . git-link-dispatch)))

(use-package git-modes
  :disabled) ;; Long load time.

(use-package git-timemachine
  :disabled ;; never really used. ;; Magit tools are preferred.
  :bind (:map global-leader-map ("j t" . git-timemachine-toggle)))

(use-package gptel ;; ai, copilot, chatgpt
  ;; llm copilot chat
  ;; Copilot settings:
  ;; (setq gptel-model 'claude-3.7-sonnet)
  ;; (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((org-mode . "* @user: ")))
  (gptel-response-prefix-alist '((org-mode . "@assistant\n")))
  :bind ( :map global-leader-map
          ("i i" . gptel)
          ("i m" . gptel-menu)
          ("i A" . gptel-add)
          ("i K" . gptel-context-remove-all)
          ("i R" . gptel-rewrite)
          :map dired-mode-map
          ("A" . gptel-add)
          ("K" . gptel-context-remove-all))
  :hook
  (gptel-mode . visual-line-mode)
  ;; (gptel-mode . gptel-highlight-mode) ;; Not available for some reason.
  ;; (gptel-post-stream . gptel-auto-scroll) ;; Annoying.
  ;; (gptel-post-response . gptel-beginning-of-response) ;; Doesn't always work.
  :config
  (with-eval-after-load 'dired-mode
    (bind-keys :map dired-mode-map
               ("I" . gptel-add)))
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("C-c I" . gptel-org-set-topic))))

(use-package help
  :bind (:map help-map
              ("h" . nil)) ;; accidentally pressed too often
  :custom
  (help-window-select 'other))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("k" . helpful-key) ;; overshadowed by meow
         ("." . helpful-at-point)
         ("F" . helpful-function)))

(use-package hippie-exp
  :bind (:map global-map ("M-i" . hippie-expand))
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-list
     try-expand-line
     try-expand-dabbrev
     ;; try-expand-list-all-buffers
     try-expand-line-all-buffers
     try-expand-dabbrev-all-buffers
     ;; try-expand-whole-kill ;; use M-y instead
     ;; try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name))
  :init
  (defun hippie-expand-case-fold-advice (orig-fun &rest args)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-fold-advice))

(use-package ibuffer
  :bind ( :map global-map
          ("C-x C-b" . ibuffer))
  :custom
  (ibuffer-human-readable-size t)
  (ibuffer-expert t)
  (ibuffer-display-summary t)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-title-face 'font-lock-doc-face)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil))

(use-package imenu-list
  :bind (:map global-leader-map
              ("m i" . imenu-list)
              ("m I" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil)
  :hook
  (imenu-list-mode . hl-line-mode))

(use-package indent-bars
  :bind (:map global-leader-map
              ("m g" . indent-bars-mode))
  :hook
  (yaml-mode . indent-bars-mode))

(use-package jinx
  :disabled ;; never really used and there are compilation errors.
  :bind (("M-$" . jinx-correct)
         ([remap flyspell-mode] . jinx-mode)))

(use-package kubernetes
  :disabled ;; rarely used
  :if (and (display-graphic-p) (executable-find "kubectl"))
  :commands (kubernetes-overview)
  :bind (:map global-leader-map
              ("k O" . kubernetes-overview))
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

(use-package ligature
  :disabled ;; too much setup
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
   "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
   "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
   "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>" "<* *>" "</" "</>" "/>" "<!--"
   "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
   "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-"
   "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
   "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::"
   ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))

(use-package lsp-mode
  :disabled ;; preferred eglot
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-format-buffer] . lsp-format-buffer)
               ([remap xref-find-references] . lsp-find-references)
               ([remap eldoc] . lsp-describe-thing-at-point)))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-set-bindings))

(use-package magit
  :commands (magit-project-status)
  :bind (:map global-leader-map
              ("j" . magit-file-dispatch)
              ("J" . magit-dispatch))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" "j")))
  :custom
  (magit-blame-echo-style 'headings)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-append-suffix 'magit-file-dispatch "G" '("j" "Goto Status Here" magit-status-here)))

(use-package magit-todos
  :disabled ;; not really used, slow startup if project too big
  :bind (:map project-prefix-map ("t" . magit-todos-list))
  :config
  (magit-todos-mode 1))

(use-package marginalia
  :demand
  :custom
  (completions-detailed nil)
  :config
  (marginalia-mode))

(use-package meow
  :demand
  :custom
  (meow-use-clipboard t)
  (meow-keypad--self-insert-undefined nil)
  (meow-expand-hint-remove-delay 2)
  (meow-cursor-type-motion '(hbar . 2))
  :init
  (defun meow-search-reverse ()
    (interactive)
    (unless (meow--direction-backward-p)
      (meow-reverse))
    (call-interactively #'meow-search))
  (defun meow-setup ()
    (set-face-attribute 'meow-insert-indicator nil :inherit '(bold-italic warning))
    (set-face-attribute 'meow-beacon-indicator nil :inherit '(bold success))
    (set-face-attribute 'meow-motion-indicator nil :inherit 'italic)
    (add-to-list 'meow-expand-exclude-mode-list 'help-mode)
    (meow-motion-overwrite-define-key
     '("<escape>" . ignore))
    (meow-normal-define-key
     (cons "SPC" global-leader-map)
     '("M-DEL" . meow-backward-kill-word)
     '("M-d" . meow-kill-word)
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("_" . meow-reverse)
     '("(" . nil)
     '(")" . nil)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-sync-grab)
     '("d" . meow-delete)
     '("D" . meow-kill)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-till)
     '("F" . meow-find)
     (cons "g" goto-map)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-pop-to-mark)
     '("M" . meow-unpop-to-mark)
     '("n" . meow-search)
     '("N" . meow-search-reverse)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . nil) ;; Keep q unbound for other apps to bind.
     '("Q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-sync-grab)
     (cons "s" search-map)
     '("S" . save-buffer)
     '("t" . nil)
     '("T" . meow-swap-grab)
     '("u" . meow-undo)
     ;; '("U" . meow-undo-in-selection)
     '("U" . undo-tree-redo)
     '("v" . meow-page-down)
     '("V" . meow-page-up)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-kill-whole-line)
     '("y" . meow-save)
     '("Y" . meow-save-append)
     '("z" . meow-pop-selection)
     '("'" . meow-last-buffer)
     '(";" . meow-comment)
     '(":" . goto-line)
     '("/" . meow-visit)
     '("," . meow-inner-of-thing)
     '("<" . nil)
     '("." . meow-bounds-of-thing)
     '(">" . nil)
     '("<escape>" . meow-cancel-selection)
     '("<backspace>" . meow-backward-delete)))
  :config
  (meow-setup)
  (meow-global-mode))

(use-package ob-http
  :disabled ;; Better to use curl in org-source blocks.
  :after org)

(use-package orderless
  :custom
  (completion-styles '(substring partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package org
  :init
  (defun org-mode-setup ()
    (electric-indent-local-mode -1))
  :hook
  (org-mode . org-mode-setup)
  (org-agenda-mode . hl-line-mode)
  :bind ( :map mode-specific-map
          ("n L" . org-store-link)
          :map global-leader-map
          ("n SPC" . org-search-view)
          ("n a" . org-agenda)
          ("n f" . org-capture-goto-target)
          ("n k" . org-capture)
          ("n r" . org-occur-link-in-agenda-files)
          ("n s" . org-occur-in-agenda-files)
          ("n t" . org-todo-list)
          ("n '" . org-capture-goto-last-stored)
          ("n ," . org-mark-ring-goto)
          ("n L" . org-store-link)
          :map org-mode-map
          ([remap goto-address-at-point] . org-open-at-point)
          ([remap kill-sentence] . org-cut-subtree)
          ("M-H" . org-babel-mark-block)
          ("M-n" . org-next-visible-heading)
          ("M-p" . org-previous-visible-heading)
          ("C-M-a" . org-up-element)
          ("C-M-e" . org-down-element)
          ("M-N" . org-move-subtree-down)
          ("M-P" . org-move-subtree-up))
  :custom
  (org-directory "~/.notes")
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'far)
  (org-agenda-window-setup 'reorganize-frame)
  (org-archive-location ".archive::* From %s")
  (org-edit-src-content-indentation 0)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1)))
  (org-special-ctrl-a/e t)
  (org-agenda-sorting-strategy '((todo urgency-down category-keep deadline-up)))
  ;; https://orgmode.org/manual/Capture-templates.html
  (org-capture-templates
   `(("t" "Task" entry (file+headline "tasks.org" "Task") "* TODO %?\n%i")
     ("n" "Note" entry (file+headline "notes.org" "Note") "* %?\n%i")
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %?\n%T\n%i")
     ("J" "Journal" entry (file+olp+datetree "journal.org") "* JOURNAL\n%T\n%a\n%i"
      :immediate-finish t)))
  :config
  (custom-set-faces
   '(org-todo ((t (:weight bold :foreground "light goldenrod"))))
   '(org-done ((t (:weight bold :foreground "dim gray")))))
  (require 'org-capture)
  (require 'org-crypt)
  (unless (file-exists-p "~/.notes")
    (make-directory "~/.notes"))
  (setopt org-agenda-files (list org-directory))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)))) ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html

(use-package org-roam
  :disabled ;; mainly for networking notes, but not really used
  :after (org)
  :commands (org-roam-node-find)
  :bind ( :map global-leader-map
          ("n r c" . org-roam-capture)
          ("n r f" . org-roam-node-find)
          ("n r i" . org-roam-node-insert)
          ("n r t" . org-roam-tag-add)
          ("n r w" . org-roam-refile)
          ("n r l" . org-roam-buffer-toggle))
  :init
  (defun org-roam-mode-setup ()
    (bind-keys :map (current-local-map)
               ("C-c n i" . org-roam-node-insert)
               ("C-c n t" . org-roam-tag-add)
               ("C-c n w" . org-roam-refile)
               ("C-c n l" . org-roam-buffer-toggle)))
  :hook
  (org-mode . org-roam-mode-setup)
  :custom
  (org-roam-directory "~/.notes/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:12}" 'face 'org-tag)))
  :config
  (make-directory "~/.notes/org-roam" t)
  (org-roam-db-autosync-mode))

(use-package paredit
  :disabled ;; Gets in the way, difficult to fix a broken parens mat
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package persistent-scratch
  :disabled ;; not really used, slower start-up
  :if (display-graphic-p)
  :defer 5
  :config
  (persistent-scratch-setup-default))

(use-package pinentry
  ;; allows for secure entry of passphrases requested by GnuPG
  :after magit
  :config
  (pinentry-start))

(use-package popper
  :disabled ;; prefer display-buffer-alist
  :demand
  :if (display-graphic-p)
  :bind ( :map popper-mode-map
          ("M-`" . popper-cycle))
  :custom
  (popper-reference-buffers
   '("^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$"  shell-mode
     "^\\*term.*\\*$"   term-mode
     "^\\*vterm.*\\*$"  vterm-mode
     "^\\*eat.*\\*$"  eat-mode
     ))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package proced
  :commands proced
  :bind (:map global-leader-map ("k P" . proced))
  :custom
  (proced-auto-update-flag 'visible)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'short))

(use-package project
  :bind ( :map project-prefix-map
          ("K" . project-forget-project))
  :custom
  (project-switch-commands '((project-find-regexp "Regexp" "g")
                             (project-find-file "File" "f")
                             (project-find-dir "Dir" "d")
                             (project-eshell "Eshell" "e")
                             (project-kill-buffers "Kill" "k")))
  :init
  (keymap-set global-leader-map "p" project-prefix-map))

(use-package show-font
  :if (display-graphic-p) ;; none exist in terminal
  :bind (:map global-leader-map
              (", X" . show-font-tabulated)))

(use-package simple-modeline
  :demand
  :init
  (require 's)
  (defun simple-modeline-segment-branch ()
    "Display current git branch in mode line."
    (when vc-mode
      (let ((branch (s-truncate 30 vc-mode)))
        (propertize (format " %s" branch) 'face 'font-lock-keyword-face))))
  (defun simple-modeline-segment-project-name ()
    "Display project name in mode line."
    (if (project-current)
        (propertize (format "[%s]" (project-name (project-current))) 'face 'bold)))
  (defun simple-modeline-segment-buffer-name-2 ()
    "Display buffer's relative-name in mode line."
    (propertize (concat "  " (mode-line-buffer-name)) 'face 'mode-line-buffer-id))
  (defun simple-modeline-segment-spaces ()
    (propertize "  "))
  (defun mode-line-buffer-name ()
    (if (buffer-file-name)
        (string-truncate-left (relative-file-name) 50)
      (buffer-name)))
  :custom
  (simple-modeline-segments
   '(( ;; left indicators
      meow-indicator
      simple-modeline-segment-modified
      simple-modeline-segment-spaces
      simple-modeline-segment-project-name
      ;; simple-modeline-segment-buffer-name
      simple-modeline-segment-buffer-name-2
      simple-modeline-segment-position)
     ( ;; right indicators
      ;; simple-modeline-segment-minor-modes
      ;; simple-modeline-segment-input-method
      ;; simple-modeline-segment-eol
      ;; simple-modeline-segment-encoding
      ;; simple-modeline-segment-vc
      simple-modeline-segment-branch
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode
      simple-modeline-segment-spaces)))
  :config
  (simple-modeline-mode))

(use-package spacious-padding
  :if (display-graphic-p)
  :bind ( :map global-leader-map
          ("m p" . spacious-padding-mode)))


(use-package tab-bar
  :if (display-graphic-p)
  :bind ( :map goto-map
          ("T" . tab-bar-mode)
          ("t SPC" . tab-bar-switch-to-tab)
          ("t q" . tab-bar-close-tab)
          ("t \"" . tab-bar-switch-to-last-tab)
          ("t t" . tab-bar-new-tab)
          ("t T" . tab-bar-undo-close-tab)
          ("t n" . tab-bar-switch-to-next-tab)
          ("t p" . tab-bar-switch-to-prev-tab)
          ("t N" . tab-bar-move-tab)
          ("t P" . tab-bar-move-tab-backward)
          ("t r" . tab-bar-rename-tab)
          ("t '" . tab-bar-switch-to-recent-tab))
  :init
  (defun tab-bar-tab-name-project ()
    (if (project-current)
        (propertize (format "[%s] " (project-name (project-current))) 'face 'bold)
      (tab-bar-tab-name-current)))
  :custom
  (tab-bar-show 1)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-button-show nil)
  (tab-bar-close-button nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-button nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-project)
  :config
  (custom-set-faces ;; faces must be set in config since not available on init.
   '(tab-bar-tab ((t (:weight bold))))
   '(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground "gray50"))))))

(use-package tmr
  :bind (:map global-leader-map
              ("m t" . tmr-tabulated-view))
  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer tmr-acknowledge-minibuffer))
  :config
  (tmr-mode-line-mode t))

(use-package transient
  ;; needed by magit, forge, and others
  :defer)

(use-package trashed
  :disabled ;; Never used.
  :bind (:map global-leader-map
              ("m _" . trashed))
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treemacs
  :disabled ;; Prefer builtin dired.
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map global-leader-map
              ("m p" . treemacs-select-window)
              ("m P" . treemacs))
  :custom
  (treemacs-no-png-images t)
  (treemacs-hide-dot-git-directory t)
  :config
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t))

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (gleam "https://github.com/gleam-lang/tree-sitter-gleam/")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
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

(use-package undo-tree
  :demand
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo-tree-history"))))
  :config
  (global-undo-tree-mode 1))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package vertico-posframe
  :if (display-graphic-p)
  :defer
  :after vertico
  :bind ( :map global-leader-map
          ("m v" . vertico-posframe-mode))
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-min-width 80))

(use-package visual-replace
  :demand
  :bind ( :map search-map
          ("%" . visual-replace-selected))
  :config
  (visual-replace-global-mode))

(use-package vterm
  ;; https://github.com/akermu/emacs-libvterm/blob/master/README.md
  ;; In config.fish
  ;; if test "$INSIDE_EMACS" = 'vterm'
  ;;   and test -n "$EMACS_VTERM_PATH"
  ;;   and test -d "$EMACS_VTERM_PATH"
  ;;   source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
  ;; end
  :if (and (display-graphic-p)
           (string-suffix-p "/bin/fish" (getenv "SHELL")))
  :bind ( :map global-leader-map
          ("k t" . vterm-project)
          ("k T" . vterm))
  :init
  (defun vterm-project ()
    (interactive)
    (require 'vterm) ;; resolves: Defining as dynamic an already lexical var: vterm-buffer-name
    (let ((vterm-buffer-name
           (or (and (project-current)
                    (format "*%s-vterm*" (project-name (project-current))))
               "*vterm*"))
          (default-directory (or (project-directory) default-directory)))
      (vterm)))
  :custom
  (vterm-always-compile-module t)
  (vterm-copy-exclude-prompt t)
  (vterm-kill-buffer-on-exit t)
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000) ;; can't go higher than this
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*vterm\\*" (display-buffer-in-side-window) (window-height . 0.3))))

(use-package which-key
  :demand
  :custom
  (which-key-side-window-location 'right)
  :config
  (which-key-mode))

(use-package whisper
  :if (executable-find "ffmpeg")
  :bind (("C-M-y" . whisper-run))
  :init
  (defun rk/get-ffmpeg-device ()
    "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
    (unless (string-equal system-type "darwin")
      (error "This function is currently only supported on macOS"))
    (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
      (cl-loop with at-video-devices = nil
               with at-audio-devices = nil
               with video-devices = nil
               with audio-devices = nil
               for line in lines
               when (string-match "AVFoundation video devices:" line)
               do (setq at-video-devices t
                        at-audio-devices nil)
               when (string-match "AVFoundation audio devices:" line)
               do (setq at-audio-devices t
                        at-video-devices nil)
               when (and at-video-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
               when (and at-audio-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
               finally return (list (nreverse video-devices) (nreverse audio-devices)))))
  (defun rk/find-device-matching (string type)
    "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
    (let* ((devices (rk/get-ffmpeg-device))
           (device-list (if (eq type :video)
                            (car devices)
                          (cadr devices))))
      (cl-loop for device in device-list
               when (string-match-p string (cdr device))
               return (car device))))
  (defcustom rk/default-audio-device nil
    "The default audio device to use for whisper.el and outher audio processes."
    :type 'string)
  (defun rk/select-default-audio-device (&optional device-name)
    "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
    (interactive)
    (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
           (indexes (mapcar #'car audio-devices))
           (names (mapcar #'cdr audio-devices))
           (name (or device-name (completing-read "Select audio device: " names nil t))))
      (setq rk/default-audio-device (rk/find-device-matching name :audio))
      (when (boundp 'whisper--ffmpeg-input-device)
        (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device))))))

(use-package writeroom-mode
  :if (display-graphic-p)
  :bind ( :map global-leader-map
          ("m w" . writeroom-mode)
          ("m W" . global-writeroom-mode))
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width 120))

(use-package xclip
  :demand
  :unless (display-graphic-p)
  :config
  (xclip-mode))

(use-package xref
  :defer
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily))
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :demand
  :bind (:map goto-map
              ("&" . yas-visit-snippet-file)
              :map global-leader-map
              ("x &" . yas-new-snippet))
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :disabled ;; Better to rely on custom built templates over externals.
  :after yasnippet)
