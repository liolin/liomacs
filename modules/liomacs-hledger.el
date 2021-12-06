(defun liomacs/hledger-mode-setup ()
  (company-mode)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'hledger-company))

(use-package hledger-mode
  :bind (("C-c e" . hledger-jentry)
	 ("C-c j" . hledger-run-comand))
  :hook
  (hledger-mode . liomacs/hledger-mode-setup)
  :custom
  (hledger-jfile "/home/liolin/finance/2021.journal")
  (hledger-currency-string "CHF")
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode)))

(provide 'liomacs-hledger)
