;;; emacs-modeline.el --- Mode Line  -*- lexical-binding: t; -*-

(require 'project)

(defun emacs-modeline-emphasis-face ()
  "Return bold face for the selected window's mode line."
  (when (mode-line-window-selected-p)
    'bold))

(defun emacs-modeline-modified ()
  "Display buffer modified or read-only status in mode line."
  (unless (string-match-p "\\*.*\\*" (buffer-name))
    (propertize (cond ((and buffer-read-only (buffer-file-name))
                       (propertize "  " 'face 'error))
                      ((buffer-modified-p)
                       (propertize " ● " 'face 'font-lock-variable-name-face))
                      (t
                       (propertize " ○ " 'face 'shadow)))
                'help-echo #'mode-line-read-only-help-echo
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'mode-line-toggle-read-only))))

(defun emacs-modeline-project ()
  "Display project name in mode line."
  (when-let* ((name (project-mode-line-format))
              (shortened-name (truncate-string-to-width (string-trim-left name) 24 nil nil t)))
    (propertize (format " [%s] " shortened-name) 'face (emacs-modeline-emphasis-face))))

(defun emacs-modeline-buffer-name ()
  "Display buffer's relative-name in mode line."
  (let* ((name (or (relative-file-name) (buffer-name)))
         (shortened-name (string-truncate-left name 50)))
    (propertize (format " %s " (string-replace "%" "%%" shortened-name)))))

(defun emacs-modeline-position ()
  "Display cursor position in mode line."
  " %l:%c ")

(defun emacs-modeline-branch ()
  "Display current git branch in mode line."
  (when vc-mode
    (let ((branch (replace-regexp-in-string "\\` *[-:@!?]" "" vc-mode)))
      (format "  %s " (truncate-string-to-width branch 40)))))

(defun emacs-modeline-narrow ()
  "Display narrowing indicator in mode line."
  (propertize "%n"
              'help-echo "mouse-2: Remove narrowing from buffer"
              'mouse-face 'mode-line-highlight
              'local-map (make-mode-line-mouse-map 'mouse-2 #'mode-line-widen)))

(defun emacs-modeline-keyboard-macro ()
  "Display keyboard macro recording indicator in mode line."
  (when defining-kbd-macro
    mode-line-defining-kbd-macro))

(defun emacs-modeline-misc-info ()
  "Display narrowing, macro recording and `mode-line-misc-info' in mode line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'shadow))))
    (list (emacs-modeline-narrow)
          (emacs-modeline-keyboard-macro)
          (unless (string-empty-p misc-info)
            (string-replace "%" "%%" (truncate-string-to-width (concat " " misc-info " ") 20 nil nil "]"))))))

(defun emacs-modeline-process ()
  "Display `mode-line-process' in mode line."
  (when mode-line-process
    '(" " mode-line-process " ")))

(defun emacs-modeline-major-mode ()
  "Display major mode in mode line."
  `(" "
    (:propertize mode-name
                 face ,(emacs-modeline-emphasis-face)
                 help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
                 mouse-face mode-line-highlight
                 local-map ,mode-line-major-mode-keymap)
    " "))

(setq-default mode-line-format
              '((:eval (emacs-modeline-modified))
                (:eval (emacs-modeline-project))
                (:eval (emacs-modeline-buffer-name))
                (:eval (emacs-modeline-position))
                mode-line-format-right-align
                (:eval (emacs-modeline-misc-info))
                (:eval (emacs-modeline-branch))
                (:eval (emacs-modeline-process))
                (:eval (emacs-modeline-major-mode))))

(provide 'emacs-modeline)
;;; emacs-modeline.el ends here
