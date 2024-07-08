(use-package popper
  :unless use-minimal-emacs
  :demand t
  :bind (:map toggle-map
              ("o" . popper-toggle)
              ("O" . popper-toggle-type)
              ("n" . popper-cycle)
              ("p" . popper-cycle-backwards)
              ("Q" . popper-kill-latest-popup)
              :repeat-map toggle-cycle-repeat-map
              ("n" . popper-cycle)
              ("p" . popper-cycle-backwards)
              ("Q" . popper-kill-latest-popup))
  :init
  (setq popper-reference-buffers
        '(("Output\\*$" . hide)
          (completion-list-mode . hide)
          occur-mode
          "\\*Messages\\*"))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "errors\\*$"
          "\\*Async Shell Command\\*"
          special-mode
          help-mode
          compilation-mode
          comint-mode))
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                  "^\\*term.*\\*$"   term-mode   ;term as a popup
                  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  )))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vterm
  :unless use-minimal-emacs
  :defer t
  :bind (:map toggle-map
              ("t" . vterm-dwim))
  :init
  (defun vterm-dwim ()
    (interactive)
    (let ((default-directory (or (project-directory) default-directory)))
      (vterm)))
  (defun vterm-named ()
    (interactive)
    (vterm (read-string "Session name: ")))
  :custom
  (vterm-copy-mode-remove-fake-newlines t)
  (vterm-max-scrollback 10000))

(use-package xclip
  :unless (or use-minimal-emacs (display-graphic-p))
  :config
  (xclip-mode 1))
