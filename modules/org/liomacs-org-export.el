(defun liomacs/export-by-tag ()
  (interactive)
  (make-local-variable 'org-export-select-tags)
  (setq org-export-select-tags (list (read-from-minibuffer "Tag: ")))
  (org-export-dispatch))

(use-package ox-hugo
  :after ox)
