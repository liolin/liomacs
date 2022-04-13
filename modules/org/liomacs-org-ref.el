(use-package org-ref
  :bind (:map org-mode-map
	      ("C-c ]" . org-ref-insert-link-hydra/body))
  :config
  (setq org-latex-prefer-user-labels t))



;(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
;(setq org-latex-prefer-user-labels t)
