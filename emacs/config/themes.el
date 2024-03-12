(use-package gruvbox-theme)

(use-package catppuccin-theme
  :config
  ;; (catppuccin-reload)
  (setq catppuccin-flavor 'mocha)) ;; 'frappe, 'latte, 'macchiato, or 'mocha

(use-package ef-themes
  :init
  (setq ef-melissa-light-palette-overrides '((fringe unspecified))))

(use-package modus-themes
  :init
  ;; https://protesilaos.com/emacs/modus-themes
  (when (display-graphic-p)
    (setq modus-vivendi-tritanopia-palette-overrides
          '((fringe unspecified)
            (bg-line-number-active unspecified)
            (bg-line-number-inactive unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-main "#d0d0d0")
            (bg-main "#14191e")))))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-display-misc-in-all-mode-lines nil)
  (doom-modeline-env-version nil)
  :config
  (doom-modeline-mode 1))

;; LOAD THEME
(set-face-font 'default "-*-Hack Nerd Font-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")
(consult-theme 'modus-vivendi-tritanopia)
;; (consult-theme 'catppuccin)