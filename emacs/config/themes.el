(use-package emacs
  :ensure nil
  :bind (:map toggle-map
              ("b" . toggle-big-font))
  :init
  (defvar toggle-big-font-sizes '(210 140)
    "List of font sizes to toggle between.")
  (defun toggle-big-font ()
    "Toggle between the different font sizes in `toggle-big-font-sizes'."
    (interactive)
    (let ((current-size (pop toggle-big-font-sizes)))
      (add-to-list 'toggle-big-font-sizes current-size t))
    (set-face-attribute 'default nil :height (car toggle-big-font-sizes)))
  :config
  ;; (set-face-font 'default "-*-Hack Nerd Font-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  ;; (set-face-font 'default "-*-Roboto Mono-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  ;; (set-face-font 'default "-*-JetBrainsMono Nerd Font-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height (car toggle-font-big-font-sizes)
                      :weight 'regular
                      :slant 'normal
                      :width 'normal)
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 112)))

(use-package gruvbox-theme)

(use-package timu-rouge-theme)

(use-package catppuccin-theme
  :disabled t
  :config
  ;; (catppuccin-reload)
  (setq catppuccin-flavor 'mocha)) ;; 'frappe, 'latte, 'macchiato, or 'mocha

(use-package ef-themes
  :init
  (setq ef-melissa-light-palette-overrides '((fringe unspecified))))

(use-package modus-themes
  :init
  ;; https://protesilaos.com/emacs/modus-themes
  (setq modus-vivendi-tritanopia-palette-overrides
        '((fringe unspecified)
          (bg-added-fringe "SeaGreen4")
          (bg-removed-fringe "IndianRed4")
          (bg-changed-fringe "SteelBlue4")
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fg-main "#d0d0d0")
          (bg-main "#14191e"))))

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
