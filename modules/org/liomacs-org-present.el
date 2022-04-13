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
			      (org-present-read-write))))

(use-package ox-reveal)
