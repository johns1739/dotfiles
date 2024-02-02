;;; treesit-nav.el --- Utils using treesit -*- lexical-binding: t -*-

(require 'treesit)

(defgroup treesit-nav nil
  "Treesit navigation and region expansion."
  :prefix "treesit-nav-"
  :group 'convenience)

(defun treesit-nav--repeated-or-regioned-p ()
  (or (and (eq last-command this-command) (mark t))
      (region-active-p)))

(defun treesit-nav--left-pos ()
  (if (treesit-nav--repeated-or-regioned-p)
      (min (point) (mark))
    (point)))

(defun treesit-nav--right-pos ()
  (if (treesit-nav--repeated-or-regioned-p)
      (max (point) (mark))
    (point)))

(defun treesit-nav--node-surrounds-region-p (node)
  (and (> (treesit-nav--left-pos) (treesit-node-start node))
       (< (treesit-nav--right-pos) (treesit-node-end node))))

(defun treesit-nav--next-parent-node ()
  (setq current-node
        (if (treesit-nav--repeated-or-regioned-p)
            (or (treesit-parent-until current-node #'treesit-nav--node-surrounds-region-p)
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

;; TODO
;; (defun treesit-contract-region () )

(provide 'treesit-nav)
