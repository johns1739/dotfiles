(require 'package)

(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Configure ELPA priorities
;; Prefer GNU sources and stable versions before development versions from MELPA.
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                        ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                        ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                        ; from melpa

(setq package-enable-at-startup nil)
