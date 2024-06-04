;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 12)
  (lsp-eldoc-render-all t)
  :init
  (defalias 'lsp-ensure-caller #'lsp-deferred)
  (defun lsp-set-bindings ()
    (bind-keys :map (current-local-map)
               ([remap indent-buffer] . lsp-format-buffer)
               ([remap evil-lookup] . lsp-describe-thing-at-point)
               ([remap eldoc] . lsp-describe-thing-at-point)
               ([remap xref-find-references] . lsp-find-references)
               ([remap xref-find-definitions] . lsp-find-definition)
               ([remap evil-goto-definition] . lsp-find-definition)))
  :hook
  (lsp-managed-mode . lsp-set-bindings))
