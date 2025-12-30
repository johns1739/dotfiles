;;-*- lexical-binding: t; -*-

(defvar-keymap global-leader-map :doc "Global leader keymap.")
(keymap-set ctl-x-map "SPC" global-leader-map)
(keymap-set global-leader-map "g" goto-map)
(keymap-set global-leader-map "s" search-map)

(bind-keys :map global-map
           ([remap backward-sentence] . backward-sexp)
           ([remap forward-sentence] . forward-sexp)
           ([remap split-window-below] . split-window-below-and-jump)
           ([remap split-window-right] . split-window-right-and-jump)
           ([remap downcase-word] . downcase-dwim)
           ([remap upcase-word] . upcase-dwim)

           ("C-M-;" . comment-indent)
           ("C-j" . comment-indent-new-line)
           ("M-I" . completion-at-point)
           ("M-j" . join-line)
           ("M-L" . duplicate-dwim)
           ("M-n" . forward-paragraph)
           ("M-o" . other-window)
           ("M-p" . backward-paragraph)

           ;; Global Leader Bindings
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

           ;; Package Modes
           ("m <tab>" . toggle-truncate-lines)
           ("m c" . display-fill-column-indicator-mode)
           ("m C" . global-display-fill-column-indicator-mode)
           ("m h" . hl-line-mode)
           ("m H" . global-hl-line-mode)
           ("m n" . display-line-numbers-mode)
           ("m N" . global-display-line-numbers-mode)

           ;; Compilation & Computation
           ("k c" . calc)
           ("k r" . regexp-builder)

           ;; Settings (Look & Feel)
           (", ," . open-custom-file)
           (", <" . open-init-file)
           (", SPC" . load-theme)
           (", =" . global-text-scale-adjust)
           (", D" . toggle-debug-on-error)
           (", x" . describe-font)
           (", f" . toggle-frame-maximized)
           (", F" . toggle-frame-fullscreen)
           (", r" . reload-emacs)
           (", R" . restart-emacs)

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
           ("b" . ibuffer)
           ("g" . rgrep)
           ("j" . list-registers)
           ("m" . list-bookmarks)
           ("o" . occur)
           ("r" . recentf-open))

;; commands & functions & definitions

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

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

;; Load packages
(setq packages '("setup.el" "emacs.el" "packages.el" "langs.el" "color-themes.el"))
(dolist (package packages)
  (tt (format "*** %s" package)
      (load (concat user-emacs-directory "packages/" package))))

;; custom settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(tt (format "*** %s" custom-file)
    (load custom-file :no-error-if-file-missing))
