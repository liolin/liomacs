(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :hook (typescript-mode . setup-tide-mode))

(use-package web-mode
  :hook
  (web-mode . (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  (setup-tide-mode))))
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode)))

(use-package ng2-mode)

(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))

(setq lsp-clients-angular-language-server-command
  '("node"
    "/usr/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/lib/node_modules"
    "--tsProbeLocations"
    "/usr/lib/node_modules"
    "--stdio"))
(with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))

(provide 'liomacs-web)

