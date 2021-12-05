(use-package lsp-mode
  :hook
  (c++-mode . lsp)
  :config
  (require 'dap-cpptools))

(provide 'liomacs-cpp)
