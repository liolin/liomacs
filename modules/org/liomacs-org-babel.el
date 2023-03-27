(use-package org
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (latex . t)
     (shell . t)
     (hledger . t)
     (python . t)))
  :custom
  (org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (org-confirm-babel-evaluate nil))
