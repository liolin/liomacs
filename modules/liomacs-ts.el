(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :hook (typescript-mode . setup-tide-mode))

(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))


(provide 'liomacs-ts)
