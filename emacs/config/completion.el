(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  :straight (corfu :files (:defaults "extensions/*.el") :includes (corfu-echo))
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-auto nil) ; Enable auto completion
  (corfu-auto-delay 0.2) ; Enable auto completion
  (corfu-auto-prefix 3) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-echo-delay 0.3)
  (corfu-preselect 'valid)
  (corfu-separator ?\s)
  :config
  (global-corfu-mode 1)
  (corfu-echo-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package cape
  ;; Cape provides Completion At Point Extensions
  :custom
  (completion-at-point-functions
   (list #'cape-dabbrev
         #'cape-abbrev
         #'cape-keyword
         #'cape-dict
         #'cape-file
         ;; #'cape-line ;; Kinda buggy
         )))

(use-package copilot
  :ensure t
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :custom
  (copilot-indent-offset-warning-disable t)
  :init
  (with-eval-after-load 'copilot
    (bind-keys :map copilot-completion-map
               ("M-n" . copilot-accept-completion-by-line)
               ("M-f" . copilot-accept-completion-by-word)
               ("<tab>" . copilot-accept-completion)
               ("TAB" . copilot-accept-completion)))
  :hook
  (prog-mode . copilot-mode))


(use-package paredit
  :disabled t
  ;; https://paredit.org/releases/26/paredit.html
  :hook
  (scheme-mode . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode))

(use-package smartparens
  :disabled t
  :config
  (require 'smartparens-config))
