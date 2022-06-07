(use-package tex
  :straight auctex
  :hook
  (TeX-mode . lsp-deferred)
  (TeX-mode . flycheck-mode)
  (TeX-mode . yas-minor-mode)
  :config
  (setq TeX-auto-save nil)
  (setq TeX-parse-self t)
  (setq TeX-master nil))

(provide 'liomacs-latex)
