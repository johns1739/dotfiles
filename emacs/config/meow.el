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
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    (meow-leader-define-key
     '("j" . "H-j")
     '("k" . "H-k")

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
     '("M-%" . meow-query-replace-regexp)
     '("^" . delete-indentation)
     '("&" . async-shell-command)
     '("(" . nil)
     '(")" . move-past-close-and-reindent)
     '("_" . meow-reverse)

     '("a" . meow-append)
     '("A" . meow-open-below)

     '("b" . meow-back-word)
     '("B" . meow-back-symbol)

     '("c" . meow-change)
     (cons "C" compilation-map)

     '("d" . meow-delete)
     '("D" . meow-kill)

     '("e" . meow-next-word)
     '("E" . meow-next-symbol)

     '("f" . meow-find)
     '("F" . nil)

     (cons "g" goto-map)
     '("G" . meow-grab)

     '("h" . meow-left)
     '("H" . mark-paragraph)

     '("i" . meow-insert)
     '("I" . meow-open-above)

     '("j" . meow-next)
     (cons "J" git-map)

     '("k" . meow-prev)
     (cons "K" diagnostics-map)

     '("l" . meow-right)
     '("L" . recenter)

     '("m" . meow-join)
     '("M" . nil)

     '("n" . meow-search)
     (cons "N" notes-map)

     '("o" . other-window)
     (cons "O" toggle-map)

     '("p" . meow-yank)
     (cons "P" project-prefix-map)

     '("q" . nil) ;; Keep q unbound for other apps to bind.
     '("Q" . meow-quit)

     '("r" . meow-replace)
     '("R" . meow-swap-grab)

     (cons "s" search-map)
     '("S" . save-buffer)

     '("t" . meow-till)
     '("T" . nil)

     '("u" . meow-undo)
     '("U" . nil)

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

     ;; Keep these unbound for other apps to bind.
     '("<tab>" . nil)
     '("<down>" . nil)
     '("<up>" . nil)
     '("<right>" . nil)
     '("<left>" . nil)

     '("\\" . cycle-spacing)
     '("|" . repeat-complex-command)

     '("'" . repeat)
     '("\"" . nil)

     '(";" . meow-comment)
     '(":" . goto-line)

     '("/" . meow-visit)
     '("?" . isearch-forward-thing-at-point)

     '("," . meow-inner-of-thing)
     '("<" . beginning-of-buffer)

     '("." . meow-bounds-of-thing)
     '(">" . end-of-buffer)

     '("[" . meow-beginning-of-thing)
     '("{" . nil)

     '("]" . meow-end-of-thing)
     '("}" . nil)

     '("`" . nil)
     '("~" . nil)

     '("<backtab>" . indent-buffer)
     '("<escape>" . meow-cancel-selection)))

  :config
  (meow-setup)
  (meow-global-mode t))
