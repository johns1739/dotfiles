;;-*- lexical-binding: t; -*-

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done)))

(setq package-enable-at-startup nil)
(setq gc-cons-percentage 0.1)
(setq gc-cons-threshold (* 16 1024 1024)) ;; 16MB

(tooltip-mode -1)
(menu-bar-mode -1)
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

