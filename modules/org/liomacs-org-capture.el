(use-package doct)          ;; Package to simplify writing org capture templates
(use-package org-cliplink)  ;; Package to copy link from clipboard in to the template

(setq liomacs/org-capture-todo-file (concat org-directory "Agenda/GTD.org"))    ;; File location for my todos
(setq liomacs/org-capture-contacts-file (concat org-directory "contacts.org"))  ;; File location for my contacts
(global-set-key (kbd "C-c X") 'org-capture)  ;; Bind org-capture

;; Setup all my org captures templates
(setq org-capture-templates
      (doct `((,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
	       :keys "t"
	       :file liomacs/org-capture-todo-file
	       :prepend t
	       :headline "Inbox"
	       :type entry
	       :template ("* TODO %?"
			  "%i %a")
	       )
	      (,(format "%s\tBookmark" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
	       :keys "b"
	       :file liomacs/org-capture-todo-file
	       :prepend t
	       :headline "Bookmark"
	       :type entry
	       :template ("* %? :%{i-type}:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
	       :i-type "web"
	       )
	      (,(format "%s\tContact" (all-the-icons-octicon "book" :face 'all-the-icons-green :v-adjust 0.01))
	       :keys "c"
	       :file liomacs/org-capture-contacts-file
	       :type entry
	       :template ("* %?\n:PROPERTIES:\n:GROUP:\n:BDAY:\n:TITLE:\n:EMAIL-P:\n:EMAIL-W:\n:PHONE-M:\n:PHONE-W:\n:PHONE-H:\n:LOCAT:\n:ANNIVERSARY:\n:NOTE:\n:END:\n\n")
	       )
	      (,(format "%s\tUniversity" (all-the-icons-faicon "graduation-cap" :face 'all-the-icons-purple :v-adjust 0.01))
	       :keys "u"
	       :file liomacs/org-capture-todo-file
	       :headline "University"
	       :prepend t
	       :type entry
	       :children ((,(format "%s\tTest" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust 0.01))
			   :keys "t"
			   :template ("* TODO [#C] %? :uni:tests:"
				      "SCHEDULED: %^{Test date:}T"
				      "%i %a"))
			  (,(format "%s\tAssignment" (all-the-icons-material "library_books" :face 'all-the-icons-orange :v-adjust 0.01))
			   :keys "a"
			   :template ("* TODO [#B] %? :uni:assignments:"
				      "DEADLINE: %^{Due date:}T"
				      "%i %a"))
			  (,(format "%s\tMiscellaneous task" (all-the-icons-faicon "list" :face 'all-the-icons-yellow :v-adjust 0.01))
			   :keys "u"
			   :template ("* TODO [#C] %? :uni:"
				      "%i %a"))
			  )
	       )
	      (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
	       :keys "e"
	       :file liomacs/org-capture-todo-file
	       :prepend t
	       :headline "Inbox"
	       :type entry
	       :template ("* TODO %? :email:"
			  "%i %a")
	       )
	      (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
	       :keys "i"
	       :file liomacs/org-capture-todo-file
	       :prepend t
	       :headline "Interesting"
	       :type entry
	       :template ("* [ ] %{desc}%? :%{i-type}:"
			  "%i %a")
	       :children ((,(format "%s\tBookmark" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
			   :keys "w"
			   :headline "Bookmark"
			   :desc "%(org-cliplink-capture) "
			   :i-type "web"
			   )
			  (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
			   :keys "a"
			   :desc ""
			   :i-type "read:reaserch"
			   )
			  (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
			   :keys "i"
			   :desc ""
			   :i-type "read:info"
			   )
			  (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
			   :keys "I"
			   :desc ""
			   :i-type "idea"
			   ))
	       )
	      (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
	       :keys "k"
	       :file liomacs/org-capture-todo-file
	       :prepend t
	       :headline "Tasks"
	       :type entry
	       :template ("* TODO %? %^G%{extra}"
			  "%i")
	       :children ((,(format "%s\tGeneral Task" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
			   :keys "k"
			   :extra ""
			   )
			  (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
			   :keys "d"
			   :extra "\nDEADLINE: %^{Deadline:}t"
			   )
			  (,(format "%s\tScheduled Task" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
			   :keys "s"
			   :extra "\nSCHEDULED: %^{Start time:}t"
			   )
			  )
	       )
	      (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
	       :keys "p"
	       :type entry
	       :prepend t
	       :template ("* %{time-or-todo} %? %^G"
			  "%i"
			  "%a")
	       :children (("Project todo"
			   :keys "t"
			   :prepend nil
			   :time-or-todo "TODO"
			   :heading "Tasks"
			   :file liomacs/org-capture-todo-file)
			  ("Project note"
			   :keys "n"
			   :time-or-todo "%U"
			   :heading "Notes"
			   :file liomacs/org-capture-todo-file)
			  ("Project changelog"
			   :keys "c"
			   :time-or-todo "%U"
			   :heading "Unreleased"
			   :file liomacs/org-capture-todo-file))
	       ))))
