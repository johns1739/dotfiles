(defun goto-configs ()
  "Go to emacs configs."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))

(defun project-copy-relative-file-name ()
  "Copy file path of current buffer relative to project directory."
  (interactive)
  (kill-new (project-relative-file-name)))

(defun copy-absolute-file-name ()
  "Copy absolute file path of current buffer."
  (interactive)
  (kill-new (absolute-file-name)))

(defun rails-compile ()
  (interactive)
  (setq compile-command
        (cond ((string-match-p "_test.rb\\'" (buffer-file-name))
               (let ((linum (number-to-string (line-number-at-pos)))
                     (file-name (project-relative-file-name)))
                 (if (< (line-number-at-pos) 5)
                     (string-join (list "rails t " file-name))
                   (string-join (list "rails t " (s-concat file-name ":" linum))))))
              (t compile-command)))
  (call-interactively #'compile-dwim))

(defun rails-comint ()
  (interactive)
  (universal-argument)
  (command-execute #'rails-compile))

(defun compile-dwim ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

(defun comint ()
  (interactive)
  (universal-argument)
  (command-execute #'compile-dwim))

(defun vterm-named ()
  (interactive)
  (vterm (read-string "Session name: ")))

(defun project-directory ()
  "Current project directory."
  (project-root (project-current)))

(defun project-relative-file-name ()
  "Relative project path to file."
  (file-relative-name (buffer-file-name) (project-directory)))

(defun absolute-file-name ()
  "Absolute path to file."
  (expand-file-name (buffer-file-name)))

(defun project-expand-path (file-name)
  (f-join (project-directory) file-name))
