;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/

(defvar use-minimal-emacs (not (display-graphic-p))
  "Load minimal emacs configuration.")

(setq gc-cons-percentage 0.2)
(setq gc-cons-threshold (* 16 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

(setq use-package-verbose t)
(setq use-package-compute-statistics t) ;; use-package-report
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(setq config-modules
      (if use-minimal-emacs
          '("config/emacs-core.el")
        '("config/emacs-core.el"
          "config/emacs-extras.el"
          "config/packages.el")))

(dolist (config-file config-modules)
  (load (locate-user-emacs-file config-file)))
