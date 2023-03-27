;; (defun liomacs/hledger-mode-setup ()
;;   (company-mode)
;;   (make-local-variable 'company-backends)
;;   (add-to-list 'company-backends 'hledger-company))

;; (use-package hledger-mode
;;   :bind (("C-c e" . hledger-jentry)
;; 	 ("C-c j" . hledger-run-comand))
;;   :hook
;;   (hledger-mode . liomacs/hledger-mode-setup)
;;   :custom
;;   (hledger-jfile "/home/liolin/org/finance/current.journal")
;;   (hledger-currency-string "CHF")
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode)))


(use-package ledger-mode
  :hook
  (ledger-mode . company-mode)
  :custom
  (ledger-mode-should-check-version nil)
  (ledger-report-links-in-register nil)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-binary-path "hledger")
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode)))

(provide 'liomacs-hledger)
