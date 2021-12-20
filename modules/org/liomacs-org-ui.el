(defun liomacs/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))))

(defun liomacs/org-mode-visual-fill ()
  "Set up visual fill mode in org mode"
  (setq visual-fill-column-width 150
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :config
  (liomacs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode . liomacs/org-mode-visual-fill))
