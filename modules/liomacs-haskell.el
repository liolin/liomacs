(use-package haskell-mode)

;; (use-package hs-lint)

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-literate-mode . lsp))

(use-package direnv
  :config
  (direnv-mode))

;; (use-package dante
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;   (add-hook 'haskell-mode-hook 'dante-mode))

(provide 'liomacs-haskell)
