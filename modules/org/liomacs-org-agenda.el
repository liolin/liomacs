(setq liomacs/org-agenda-todo-view
      `(" " "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning-days 365)))
	 (todo "TODO"
	       ((org-agenda-overriding-header "To Refile")
		(org-agenda-files "~/org/Agenda/inbox.org")))
	 (todo "TODO"
	       ((org-agenda-overriding-header "Emails")
		(org-agenda-files "~/org/Agenda/emails.org")))
         (todo "TODO"
               ((org-agenda-overriding-header "SA")
                (org-agenda-files "~/org/Agenda/sa.org")))
	 )
	))

(use-package org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files (append 
		     (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-custom-commands liomacs/org-agenda-todo-view
   '(("u" "Agenda / ToDo list for uni"
      ((tags-todo "+uni")
       (agenda "")))))
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t))
