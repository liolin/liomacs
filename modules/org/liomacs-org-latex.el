(use-package org
  :custom
  (org-latex-title-command "")
  (org-latex-toc-command "")
  (org-latex-listings 't)
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o -bibtex %f"))
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "subcaption"))
  (add-to-list 'org-latex-classes
	       '("ost-summary"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\")))
  (add-to-list 'org-latex-classes
	       '("ost-exam-summary"
		 "\\documentclass{extarticle}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\"))))
