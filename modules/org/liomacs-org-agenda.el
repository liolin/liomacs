(use-package org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files (append 
		     '("~/org/Agenda/GTD.org"
		       "~/org/Agenda/school.org"
		       "~/org/Agenda/Events.org"
		       "~/org/Agenda/Habits.org"
		       "~/org/contacts.org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-custom-commands
   '(("u" "Agenda / ToDo list for uni"
      ((tags-todo "+uni")
       (agenda "")))))
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t))
