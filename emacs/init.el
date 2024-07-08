;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/

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

(defvar use-minimal-emacs (not (display-graphic-p)) "Load minimal emacs configuration.")
(setq module-file-names
      '("config/emacs.el"

        ;; Package setup
        "config/package-manager.el"

        ;; Essentials
        "config/core-essentials.el"
        "config/goto.el"
        "config/search.el"
        "config/completion.el"
        "config/compilation.el"
        "config/diagnostics.el"
        "config/git.el"
        "config/notes.el"
        "config/toggle.el"

        ;; Languages
        "config/lsp.el"
        "config/ruby-major-mode.el"
        "config/elixir-major-mode.el"
        "config/major-modes.el"

        ;; Utils
        "config/debug-tools.el"
        "config/themes.el"
        "config/meow.el"
        "custom.el"))

(dolist (config-file-name module-file-names)
  (message "\n\nLoading %s" config-file-name)
  (benchmark-progn
    (load (locate-user-emacs-file config-file-name))))
