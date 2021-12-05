(use-package org
  :bind
  ("C-c a" . org-agenda-list)
  :custom
  (org-agenda-files (append 
		     '("~/org/Agenda/GTD.org"
		       "~/org/Agenda/Events.org"
		       "~/org/Agenda/Habits.org"
		       "~/org/contacts.org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-custom-commands
   '(("u" "Agenda / ToDo list for uni"
      ((tags-todo "+uni")
       (agenda "")))))
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (evil-set-initial-state 'org-agenda-mode 'normal))
  ;; (evil-collection-define-key 'normal 'org-agenda-mode-map
  ;;   "l" 'org-agenda-later
  ;;   "e" 'org-agenda-earlier
  ;;   "d" 'org-agenda-day-view
  ;;   "w" 'org-agenda-week-view
  ;;   "t" 'org-agenda-todo
  ;;   "gr" 'org-agenda-redo))
