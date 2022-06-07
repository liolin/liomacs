(use-package rustic
  :bind (:map rustic-mode-map
	      ("C-c C-c j" . hs-show-block)
	      ("C-c C-c J" . hs-show-all)
	      ("C-c C-c k" . hs-hide-block)
	      ("C-c C-c K" . hs-hide-all)
	      ("C-c C-c i" . lsp-ui-imenu)
	      ("C-c C-c l" . flycheck-list-errors)
	      ("C-c C-c a" . lsp-execute-code-action)
	      ("C-c C-c r" . lsp-rename)
	      ("C-c C-c q" . lsp-workspace-restart)
	      ("C-c C-c Q" . lsp-workspace-shutdown)
	      ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  :hook
  (rustic-mode . lsp-deferred)
  (rustic-mode . hs-minor-mode)
  (rustic-mode . electric-pair-mode))

(use-package lsp-mode
  :custom
  (lsp-rust-analyzer-cargo-watch-command "check"))

(provide 'liomacs-rust)
