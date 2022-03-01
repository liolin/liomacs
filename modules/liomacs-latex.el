(use-package tex
  :straight auctex
  :hook
  (TeX-mode . lsp-deferred)
  (TeX-mode . flycheck-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil))

(provide 'liomacs-latex)