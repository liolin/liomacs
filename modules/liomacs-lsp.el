(defun liomacs/setup-lsp-hook ()
  (add-hook 'prog-mode-hook (lambda () (lsp-mode 1)))
  (dolist (mode '(emacs-lisp-mode-hook))
    (add-hook mode (lambda () (lsp-mode -1)))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (prog-mode . lsp)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (liomacs/setup-lsp-hook)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui 
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(provide 'liomacs-lsp)
