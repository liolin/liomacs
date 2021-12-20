(use-package flycheck)

(use-package lsp-ltex
  :after lsp
  :ensure t
  :config
  (flycheck-add-next-checker 'lsp 'proselint)
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp))))

(provide 'liomacs-diagnostics)
