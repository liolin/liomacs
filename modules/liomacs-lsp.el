(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui 
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(provide 'liomacs-lsp)
