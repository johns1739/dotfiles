;;; -*- lexical-binding: t -*-

(setopt straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt straight-use-package-by-default t)

;; Install early for downstream dependencies

(use-package exec-path-from-shell
  :demand
  ;; If issues with git read/write access:
  ;; (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  ;; (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  :if (and (memq window-system '(mac ns x)) (display-graphic-p))
  :custom
  (exec-path-from-shell-debug t)
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
