(defun liomacs/org-export-latex-no-toc (depth)
  (message "Hello world")
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
	    depth)))

(use-package org
  :custom
  (org-latex-toc-command "")
  (org-latex-listings 't)
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o -bibtex %f"))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

