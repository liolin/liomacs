(defun liomacs/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))))
  ;;   (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-column nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-column-title nil :inherit 'fixed-pitch))

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
