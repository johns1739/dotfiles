;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/

(defvar use-minimal-emacs nil
  "Load minimal emacs configuration.")

(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(setq gc-cons-percentage 0.2)
(setq gc-cons-threshold (* 16 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

(load (locate-user-emacs-file "config/emacs-core.el"))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Custom Settings

;; (setq xref-search-program 'ripgrep)
;; (setq find-program 'fd)
;; (setq use-minimal-emacs nil)

;; (unless use-minimal-emacs
;;   (load (locate-user-emacs-file "config/packages.el"))
;;   (load-theme 'gruber-darker t)

;;   (when (display-graphic-p)
;;     (set-face-attribute 'default nil
;;                         :family "JetBrainsMono Nerd Font"
;;                         :height (car toggle-big-font-sizes)
;;                         :weight 'light ;; thin, light, medium, regular
;;                         :slant 'normal ;; italic, oblique, normal, roman
;;                         :width 'normal)
;;     (add-to-list 'default-frame-alist '(height . 50))
;;     (add-to-list 'default-frame-alist '(width . 112))))
