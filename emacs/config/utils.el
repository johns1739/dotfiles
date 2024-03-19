(use-package vertico
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :defer t)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (let ((undo-tree-history-directory (locate-user-emacs-file "undo-tree-history")))
    (unless (file-exists-p undo-tree-history-directory)
      (make-directory undo-tree-history-directory))
    (setq undo-tree-history-directory-alist
          `(("." . ,undo-tree-history-directory))))
  (global-undo-tree-mode 1))

(use-package vterm
  :defer t
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-scope 'project))

(use-package rg
  :defer t)

(use-package avy
  ;; Jump to text
  :defer t)

(use-package multiple-cursors
  :defer t)

(use-package expand-region
  :defer t)

(use-package flycheck
  ;; https://www.flycheck.org/en/latest/
  :defer t
  :custom
  (flycheck-indication-mode 'right-fringe))

(use-package consult-flycheck
  :defer t)

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :init
  (setq yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package which-key
  :config
  (which-key-mode))

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(use-package ace-window
  ;; Jump to a window
  :defer t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "GOPATH"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"
                 "TIINGO_API_TOKEN" "RUBYOPT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

(use-package paredit
  ;; http://pub.gajendra.net/src/paredit-refcard.pdf
  :disabled t
  :hook
  (scheme-mode . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode))
