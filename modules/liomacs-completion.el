(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1))


(use-package vertico
  :bind (:map vertico-map
	 ("C-j" . vertico-next)
	 ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :config
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark)
(use-package embark-consult)

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	 ("<tab>" . company-complete-selection)
	 :map prog-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'liomacs-completion)
