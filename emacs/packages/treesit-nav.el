;;; treesit-nav.el --- Utils using treesit -*- lexical-binding: t -*-

(defgroup treesit-nav nil
  "Some description"
  :prefix "treesit-nav-"
  :group 'convenience)

(defun treesit-nav--repeated-with-active-mark ()
  (and (eq last-command this-command) (mark t)))

(defun treesit-nav--next-parent-node ()
  (setq current-node
        (if (treesit-nav--repeated-with-active-mark)
            (or (treesit-node-parent current-node) current-node)
          (treesit-node-at (point)))))

(defun treesit-nav--next-node ()
  (setq current-node
        (if (treesit-nav--repeated-with-active-mark)
            (or (treesit-search-forward current-node
                                        (lambda (n) (> (treesit-node-end n)
                                                       (treesit-node-end current-node))))
                current-node)
          (treesit-node-at (point)))))

(defun treesit-nav--prev-node ()
  (setq current-node
        (if (treesit-nav--repeated-with-active-mark)
            (or (treesit-search-forward current-node
                                        (lambda (n) (< (treesit-node-start n)
                                                       (treesit-node-start current-node)))
                                        t) ;; backwards
                current-node)
          (treesit-node-at (point)))))

;;;###autoload
(defun treesit-nav-expand-region ()
  "Expand selection under cursor using treesit nodes."
  (interactive)
  (let ((node (treesit-nav--next-parent-node)))
    (goto-char (treesit-node-start node))
    (save-excursion
      (push-mark
       (goto-char (treesit-node-end node))
       nil t))))

;;;###autoload
(defun treesit-nav-expand-right ()
  "Expand to the right of cursor using treesit nodes."
  (interactive)
  (let ((node (treesit-nav--next-node)))
    (save-excursion
      (push-mark
       (goto-char (treesit-node-end node))
       nil t))))

;;;###autoload
(defun treesit-nav-expand-left ()
  "Expand to the right of cursor using treesit nodes."
  (interactive)
  (let ((node (treesit-nav--prev-node)))
    (save-excursion
      (push-mark
       (goto-char (treesit-node-start node))
       nil t))))
