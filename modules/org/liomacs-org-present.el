(use-package org-present
  :hook
  (org-present-mode . (lambda ()
			 (org-present-big)
			 (org-display-inline-images)
			 (org-present-hide-cursor)
			 (org-present-read-only)))

  (org-present-mode-quite . (lambda ()
			       (org-present-small)
			       (org-remove-inline-images)
			       (org-present-show-cursor)
			       (org-present-read-write)))
  :bind( :map org-mode-map)
  ("C-j"    . org-present-next)
  ("C-k"    . org-present-prev))
