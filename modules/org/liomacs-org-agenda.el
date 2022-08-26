(use-package org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files (append 
		     (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-custom-commands
   '(("u" "Agenda / ToDo list for uni"
      ((tags-todo "+uni")
       (agenda "")))))
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t))
