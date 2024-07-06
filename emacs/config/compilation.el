(use-package emacs
  :bind (:map compilation-map
              ("!" . project-async-shell-command)
              ("." . eval-defun)
              ("b" . eval-buffer)
              ("c" . compile-dwim)
              ("e" . eval-last-sexp)
              ("E" . eval-print-last-sexp)
              ("i" . comint)
              ("r" . recompile)
              ("v" . eval-region))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (compilation-max-output-line-length 200)
  :init
  (defun compile-dwim ()
    (interactive)
    (if (project-current)
        (call-interactively #'project-compile)
      (call-interactively #'compile)))
  (defun comint ()
    (interactive)
    (universal-argument)
    (command-execute #'compile-dwim))
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '(
                 "SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "GOPATH"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"
                 "TIINGO_API_TOKEN"
                 "RUBYOPT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
