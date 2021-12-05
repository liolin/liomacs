(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(provide 'liomacs-undo)
