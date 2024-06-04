(use-package meow
  :custom
  (meow-use-clipboard t)
  (meow-visit-collect-min-length 1)
  (meow-keypad--self-insert-undefined nil)
  (meow-expand-hint-remove-delay 1.5)
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("=" . er/expand-region)
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)

     '("+" . nil)
     '("!" . meow-kmacro-lines)
     '("@" . meow-start-kmacro-or-insert-counter)
     '("#" . meow-end-or-call-kmacro)
     '("$" . meow-kmacro-matches)
     '("%" . meow-query-replace)
     '("^" . nil)
     '("&" . async-shell-command)
     '("*" . meow-query-replace-regexp)
     '("(" . nil)
     '(")" . nil)
     '("_" . meow-reverse)

     '("a" . meow-append)
     '("A" . meow-open-below)

     '("b" . meow-back-word)
     '("B" . meow-back-symbol)

     '("c" . meow-change)
     '("C" . compile-dwim)

     '("d" . meow-delete)
     '("D" . meow-backword-delete)

     '("e" . meow-next-word)
     '("E" . meow-next-symbol)

     '("f" . meow-till)
     '("F" . meow-find)

     (cons "g" goto-map)
     '("G" . meow-grab)

     '("h" . meow-left)
     '("H" . nil)

     '("i" . meow-insert)
     '("I" . meow-open-above)

     '("j" . meow-next)
     '("J" . magit-blame-addition)

     '("k" . meow-prev)
     '("K" . nil)

     '("l" . meow-right)
     '("L" . recenter)

     '("m" . meow-join)
     '("M" . meow-back-to-indentation)

     '("n" . meow-search)
     (cons "N" notes-map)

     '("o" . meow-pop-marker)
     '("O" . other-window)

     '("p" . meow-yank)
     '("P" . meow-yank-pop)

     ;; Keep q unbound for other apps to bind.
     '("q" . nil)
     '("Q" . meow-quit)

     '("r" . meow-replace)
     '("R" . meow-swap-grab)

     '("s" . meow-kill)
     '("S" . save-buffer)

     (cons "t" toggle-map)
     '("T" . project-vterm)

     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)

     '("v" . meow-page-down)
     '("V" . meow-page-up)

     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)

     '("x" . meow-line)
     '("X" . meow-kill-whole-line)

     '("y" . meow-save)
     '("Y" . meow-save-append)

     '("z" . meow-pop-selection)
     '("Z" . meow-sync-grab)

     '("<down>" . meow-next)
     '("S-<down>" . meow-next-expand)

     '("<up>" . meow-prev)
     '("S-<up>" . meow-prev-expand)

     '("<right>" . meow-right)
     '("S-<right>" . meow-right-expand)

     '("<left>" . meow-left)
     '("S-<left>" . meow-left-expand)

     '("\\" . repeat-complex-command)
     '("|" . nil)

     '("'" . repeat)
     '("\"" . nil)

     '(";" . meow-comment)
     '(":" . meow-goto-line)

     '("/" . meow-visit)
     '("?" . nil)

     '("," . meow-inner-of-thing)
     '("<" . meow-begin-of-buffer)

     '("." . meow-bounds-of-thing)
     '(">" . meow-end-of-buffer)

     '("[" . meow-beginning-of-thing)
     '("{" . nil)

     '("]" . meow-end-of-thing)
     '("}" . nil)

     '("`" . nil)
     '("~" . nil)

     '("<tab>" . meow-indent)
     '("<escape>" . meow-cancel-selection)))
  :config
  (meow-setup)
  (meow-global-mode t))
