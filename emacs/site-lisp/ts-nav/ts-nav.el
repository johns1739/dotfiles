;;; ts-nav.el --- Utils using treesit -*- lexical-binding: t -*-

(require 'treesit)

(defgroup ts-nav nil
  "Treesit navigation and region expansion."
  :prefix "ts-nav-"
  :group 'convenience)

(defun ts-nav--repeated-p ()
  (and (eq last-command this-command) (mark t)))

(defun ts-nav--left-pos ()
  (cond ((ts-nav--repeated-p) (min (point) (mark)))
        ((region-active-p) (region-beginning))
        (t (point))))

(defun ts-nav--right-pos ()
  (cond ((ts-nav--repeated-p) (max (point) (mark)))
        ((region-active-p) (region-end))
        (t (point))))

(defun ts-nav--node-surrounds-region-p (node)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node))
        (lpos (ts-nav--left-pos))
        (rpos (ts-nav--right-pos)))
    (and
     (or (> lpos node-start) (< rpos node-end))
     (>= lpos node-start)
     (<= rpos node-end))))

(defun ts-nav--node-at-point-or-region ()
  (if (region-active-p)
      (treesit-parent-until (treesit-node-at (point)) #'ts-nav--node-surrounds-region-p)
    (treesit-node-at (point))))

(defun ts-nav--next-parent-node ()
  (setq current-node
        (if (ts-nav--repeated-p)
            (or (treesit-parent-until current-node #'ts-nav--node-surrounds-region-p)
                current-node)
          (ts-nav--node-at-point-or-region))))

(defun ts-nav-maybe-set-default-keybindings ()
  (interactive)
  (when treesit-defun-type-regexp
    (keymap-local-set "M-o" #'ts-nav-expand-region)))

(defun ts-nav--mark-region (start end)
  (if (region-active-p)
      (deactivate-mark))
  (goto-char start)
  (save-excursion
    (push-mark (goto-char end) nil t)))

;;;###autoload
(defun ts-nav-expand-region ()
  "Expand selection under cursor using treesit nodes."
  (interactive)
  (let ((node (ts-nav--next-parent-node)))
    (ts-nav--mark-region (treesit-node-start node) (treesit-node-end node))))

;; TODO
;; (defun treesit-contract-region () )

(provide 'ts-nav)
