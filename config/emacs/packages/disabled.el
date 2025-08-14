;;; -*- lexical-binding: t -*-

(use-package command-log-mode
  :bind (:map global-leader-map
              ("o l" . clm/toggle-command-log-buffer))
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

(use-package dashboard
  :demand
  :if (display-graphic-p)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

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

(use-package diff-hl
  ;; not really used, better to use magit-diff.
  :bind (:map global-leader-map
              ("j D" . diff-hl-show-hunk))
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Terminal does not have a fringe, so use margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (global-diff-hl-mode))

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

(use-package embark-consult
  ;; Not sure when this is helpful. What's the use case?
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package forge
  ;; a bit slow, not really used.
  :commands (forge-dispatch)
  :custom
  (auth-sources '("~/.authinfo")))

(use-package git-timemachine
  ;; never really used.
  ;; Magit tools are preferred.
  :bind (:map global-leader-map ("j t" . git-timemachine-toggle)))

(use-package jinx
  ;; never really used.
  ;; Requires OS dependencies.
  :bind (("M-$" . jinx-correct)
         ([remap flyspell-mode] . jinx-mode)))

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

(use-package treemacs
  ;; Prefer emacs dired.
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map global-leader-map
              ("o p" . treemacs-select-window)
              ("o P" . treemacs))
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

(use-package yasnippet-snippets
  ;; Better to rely on custom built templates over externals.
  :after yasnippet)
