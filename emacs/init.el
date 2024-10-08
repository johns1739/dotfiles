;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/

(defvar use-minimal-emacs (not (display-graphic-p))
  "Load minimal emacs configuration.")
(setq use-minimal-emacs nil)

(setq gc-cons-percentage 0.2)
(setq gc-cons-threshold (* 16 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

(setq use-package-verbose t)
(setq use-package-compute-statistics t) ;; use-package-report

(setq config-modules
      (if use-minimal-emacs
          '("config/emacs-core.el")
        '("config/emacs-core.el"
          "config/emacs-extras.el"
          "config/packages.el")))

(dolist (config-file config-modules)
  (load (locate-user-emacs-file config-file)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Font snippet, can be added to custom.el
;; Example:
;; (when (display-graphic-p)
;;   (set-face-attribute 'default nil
;;                       :family "JetBrainsMono Nerd Font"
;;                       :height (car toggle-big-font-sizes)
;;                       :weight 'light ;; thin, light, medium, regular
;;                       :slant 'normal ;; italic, oblique, normal, roman
;;                       :width 'normal)
;;   (add-to-list 'default-frame-alist '(height . 50))
;;   (add-to-list 'default-frame-alist '(width . 112)))
