(use-package undo-tree
  :after evil
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(provide 'liomacs-undo)
