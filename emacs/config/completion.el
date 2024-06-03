(use-package corfu
  :disabled t
  ;; Corfu enhances in-buffer completion with a small completion popup.
  :straight (corfu :files (:defaults "extensions/*.el") :includes (corfu-echo))
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-delay 0.5) ; Enable auto completion
  (corfu-auto-prefix 2) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-echo-delay 0.3)
  (corfu-preselect 'valid)
  (corfu-separator ?\s)
  :config
  (global-corfu-mode 1)
  (corfu-echo-mode 1))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :bind (:map completion-map
              ("." . cape-dabbrev)
              ("a" . cape-abbrev)
              ("e" . cape-elisp-block)
              ("f" . cape-file)
              ("h" . cape-history)
              ("k" . cape-keyword)
              ("l" . cape-line)
              ("s" . cape-elisp-symbol)
              ("d" . cape-dict))
  :custom
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-file
         #'cape-dict
         #'cape-elisp-symbol
         ;; #'cape-line ;; Kinda buggy
         )))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (:map copilot-completion-map
              ("M-n" . copilot-accept-completion-by-line)
              ("M-f" . copilot-accept-completion-by-word)
              ("<tab>" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion)
              ("M-e" . copilot-accept-completion))
  :custom
  (copilot-indent-offset-warning-disable t)
  :hook
  (prog-mode . copilot-mode))

(use-package yasnippet
  ;; https://joaotavora.github.io/yasnippet/index.html
  :custom
  (yas-snippet-dirs `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)
