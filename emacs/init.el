;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el


(load (locate-user-emacs-file "core.el") 'noerror)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)
