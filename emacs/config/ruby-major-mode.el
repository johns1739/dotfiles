(use-package ruby-ts-mode
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))

  (defun rails-compile ()
    (interactive)
    (setq compile-command
          (cond ((string-match-p "_test.rb\\'" (buffer-file-name))
                 (let ((linum (number-to-string (line-number-at-pos)))
                       (file-name (relative-file-name)))
                   (if (< (line-number-at-pos) 5)
                       (string-join (list "rails t " file-name))
                     (string-join (list "rails t " (s-concat file-name ":" linum))))))
                (t compile-command)))
    (call-interactively #'compile-dwim))

  (defun rails-comint ()
    (interactive)
    (universal-argument)
    (command-execute #'rails-compile))

  (defun ruby-setup ()
    (setq compile-command "rails t")
    (setq outline-regexp "\s*\\(context \\|describe \\|test \\|it \\)")
    (bind-keys :map (current-local-map)
               ([remap compile-dwim] . rails-compile)
               ([remap comint] . rails-comint)))

  (with-eval-after-load 'compile
    (push 'minitest-test compilation-error-regexp-alist)
    (push '(minitest-test "^Failure:\n.*\\[\\([^:]+\\):\\([0-9]+\\)?\\]"
                          1 ;; file
                          2 ;; line
                          nil ;; col
                          nil ;; type
                          1 ;;hyperlink
                          )
          compilation-error-regexp-alist-alist))
  :hook
  ;;(ruby-ts-mode . display-fill-column-indicator-mode)
  (ruby-base-mode . ruby-setup)
  (ruby-ts-mode . lsp-ensure-caller))
