(use-package haskell-mode)
(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "/home/liolin/.ghcup/bin/haskell-language-server-wrapper")
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-literate-mode . lsp))

(provide 'liomacs-haskell)
