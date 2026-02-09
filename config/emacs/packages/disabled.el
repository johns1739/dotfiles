;;; -*- lexical-binding: t -*-

(use-package aidermacs ;; too expensive
  :if (and (display-graphic-p) (executable-find "aider"))
  :bind ( :map global-leader-map
          ("a" . aidermacs-transient-menu))
  :custom
  ;; (aidermacs-default-model "gpt-5.2")
  ;; (aidermacs-default-model "gemini-2.5-pro"))
  (aidermacs-default-chat-mode 'architect))

(use-package casual
  ;; Too much configuration for different modes.
  :bind ( :map org-agenda-mode-map
          ("C-o" . casual-agenda-tmenu)))

(use-package command-log-mode
  :bind (:map global-leader-map
              ("m l" . clm/toggle-command-log-buffer))
  :config
  (global-command-log-mode))

(use-package consult-flycheck
  :commands (consult-flycheck))

(use-package consult-denote
  :bind (:map global-leader-map
              ("n d f" . consult-denote-find)
              ("n d s" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode))

(use-package copilot-chat
  ;; too slow
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :if (display-graphic-p)
  :requires copilot
  :after (request org markdown-mode copilot))

(use-package denote
  ;; prefer org default setup
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
  ;; rarely used, clunky
  :bind (:map global-leader-map
              ("m h I" . devdocs-install)
              ("m h h" . devdocs-lookup)
              ("m h s" . devdocs-search)))

(use-package dired-subtree
  ;; Not really used.
  :init
  (defun dired-subtree-setup ()
    (require 'dired-subtree))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :hook
  (dired-mode . dired-subtree-setup)
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package docker
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
  :if (display-graphic-p)
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (:map global-leader-map
              ("k t" . eat-project)
              ("k T" . eat)
              :map eat-semi-char-mode-map
              ("M-o" . other-window))
  :custom
  (process-adaptive-read-buffering t)
  (eat-term-scrollback-size nil)
  (eat-enable-auto-line-mode nil) ;; more intuitive to use semi-char mode
  :hook
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-load . eat-eshell-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*.*-eat\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-min-height . 25))))

(use-package eldoc-box
  ;; Annoying GUI
  :if (display-graphic-p)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

(use-package ellama
  :custom
  (ellama-user-nick "Juan")
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

(use-package flycheck
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

(use-package git-modes) ;; Long load time.

(use-package git-timemachine
  ;; never really used.
  ;; Magit tools are preferred.
  :bind (:map global-leader-map ("j t" . git-timemachine-toggle)))

(use-package jinx
  ;; never really used and there are compilation errors.
  :bind (("M-$" . jinx-correct)
         ([remap flyspell-mode] . jinx-mode)))

(use-package kubernetes
  :if (and (display-graphic-p) (executable-find "kubectl"))
  :commands (kubernetes-overview)
  :bind (:map global-leader-map
              ("k O" . kubernetes-overview))
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
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
  ;; preferred eglot
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

(use-package magit-todos
  ;; not really used
  :bind (:map project-prefix-map ("t" . magit-todos-list))
  :config
  (magit-todos-mode 1))

(use-package ob-http
  ;; Better to use curl in shell blocks.
  :after org)

(use-package paredit
  ;; Gets in the way, difficult to fix a broken parens mat
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package persistent-scratch
  ;; too slow to load up
  :demand
  :config
  (persistent-scratch-setup-default))

(use-package popper
  :demand
  :if (display-graphic-p)
  :bind (:map global-leader-map
              ("o o" . popper-toggle)
              ("o O" . popper-toggle-type))
  :init
  (defun popper-setup ()
    (bind-keys :map (current-local-map)
               ("Q" . popper-kill-latest-popup)))
  (setq popper-reference-buffers
        '(("Output\\*$" . hide)
          occur-mode
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "errors\\*$"
          "\\*Async Shell Command\\*"
          special-mode
          help-mode
          flymake-diagnostics-buffer-mode
          compilation-mode
          comint-mode))
  ;; Match eshell, shell, term, etc
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*.*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode
                  "^\\*vterm.*\\*$"  vterm-mode
                  "^\\*.*eat.*\\*$"  eat-mode)))
  (setq popper-window-height
        (lambda (win)
          (fit-window-to-buffer win (floor (frame-height) 3) 15)))
  :hook
  (popper-open-popup . popper-setup)
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package python-black
  ;; eglot formatter is good enough
  :if (executable-find "black")
  :after python
  :init
  (defun python-black-setup ()
    (bind-keys :map (current-local-map)
               ([remap indent-format-buffer] . python-black-buffer)))
  :hook
  (python-ts-mode . python-black-setup))

(use-package spacious-padding
  :if (display-graphic-p)
  :bind (:map global-leader-map
              ("m p" . spacious-padding-mode)))

(use-package trashed
  ;; Never used.
  :bind (:map global-leader-map
              ("m _" . trashed))
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treemacs
  ;; Prefer emacs dired.
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

(use-package vterm
  ;; Eat is a better termianl emulator.
  :if (display-graphic-p)
  :bind (:map global-leader-map
              ("o t" . vterm-project)
              ("o T" . vterm-named))
  :init
  (defun vterm-project ()
    (interactive)
    (let ((default-directory (or (project-directory) default-directory)))
      (vterm-other-window)))
  (defun vterm-named ()
    (interactive)
    (vterm (read-string "Session name: ")))
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 100000))

(use-package web-mode
  ;; Mode not very good.
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package yasnippet-snippets
  ;; Better to rely on custom built templates over externals.
  :after yasnippet)
