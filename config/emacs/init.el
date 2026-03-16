;;; init.el --- Init Configuration  -*- lexical-binding: t; -*-

;; TODO: These show up in *Messages* at terminal startup. What are they?
;; Loading warnings (native-compiled elisp)...done
;; Loading goto-addr (native-compiled elisp)...done
;; Loading pixel-scroll (native-compiled elisp)...done
;; Loading treesit (native-compiled elisp)...done

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'emacs-base)

(unless is-simple-editor
  (require 'emacs-manager)
  (require 'emacs-packages)
  (require 'emacs-color-themes)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file :no-error-if-file-missing))

(when is-simple-editor
  (setq custom-file (concat user-emacs-directory ".simple-custom.el"))
  (load custom-file :no-error-if-file-missing))

(provide 'init)
;;; init.el ends here
