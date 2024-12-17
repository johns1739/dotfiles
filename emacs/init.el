;;-*- lexical-binding: t; -*-

;; Documentation
;; https://karthinks.com/
;; https://emacsredux.com/
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el


(load (locate-user-emacs-file "core.el") 'noerror)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

