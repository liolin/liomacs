(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui 
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after ivy
  :commands lsp-treemacs-errors-list)


(provide 'liomacs-lsp)
