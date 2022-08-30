(defun liomacs/org-agenda-finalize-hook()
  (evil-normal-state))

(defun liomacs/load-org-modules ()
  (let ((org-modules-dir (expand-file-name "org" liomacs/module-dir)))
    (mapc 'load (directory-files org-modules-dir t "^[^#].*el$"))))


(defun liomacs/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   ;;(call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))


(defcustom liomacs/org-private-file "~/org/Agenda/GTD.org"
  "Org file for private related tasks"
  :type 'file
  :group 'liomacs)

(defcustom liomacs/org-school-file "~/org/Agenda/school.org"
  "Org file for school related tasks"
  :type 'file
  :group 'liomacs)

(defcustom liomacs/org-work-file "~/org/Agenda/work.org"
  "Org file for work related tasks"
  :type 'file
  :group 'liomacs)

(defcustom liomacs/org-inbox-file "~/org/Agenda/inbox.org"
  "Org inbox file"
  :type 'file
  :group 'liomacs)

(defcustom liomacs/org-email-file "~/org/Agenda/emails.org"
  "Org file for tracking emails"
  :type 'file
  :group 'liomacs)

(setq liomacs/org-agenda-todo-view
      '("i" "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning-days 365)))
	 (todo "TODO"
	       ((org-agenda-overriding-header "To Refile")
		(org-agenda-files '("~/org/Agenda/inbox.org"))))
	 (todo "TODO"
	       ((org-agenda-overriding-header "Emails")
		(org-agenda-files '("~/org/Agenda/emails.org"))))
	 (todo "TODO"
	       ((org-agenda-overriding-header "SA")
		(org-agenda-files '("~/org/Agenda/sa.org"))))
	 (todo "TODO"
	       ((org-agenda-overriding-header "OST")
		(org-agenda-files '("~/org/Agenda/school.org"))))
	 (todo "NEXT"
	       ((org-agenda-overriding-header "In Progress")
		(org-agenda-files '("~/org/Agenda/sa.org"
				    "~/org/Agenda/projects.org"
				    "~/org/Agenda/work.org"
				    "~/org/Agenda/school.org"
				    "~/org/Agenda/GTD.org"))))
	 (todo "TODO"
	       ((org-agenda-overriding-header "Projects")
		(org-agenda-files '("~/org/Agenda/projects.org"))))
	 )))

(use-package org
  :hook
  (org-agenda-finalize . liomacs/org-agenda-finalize-hook)
  :bind
  ("C-c l" . org-store-link)
  ("C-c X" . org-capture)
  ("C-c a" . org-agenda)
  :config
  (require 'org-protocol)
  (require 'org-tempo)
  (require 'org-agenda)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (add-to-list 'org-agenda-custom-commands liomacs/org-agenda-todo-view)
  (liomacs/load-org-modules)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ▾")
  (org-todo-keywords '((sequence "TODO" "NEXT" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-modules '(org-habit))
  (org-startup-folded t)
  (org-clock-sound "~/code/liomacs/ping.wav")
  (org-agenda-files (append 
		     (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-capture-templates
      `(("i" "inbox" entry (file liomacs/org-inbox-file)
         "* TODO %?")
	("e" "email" entry (file+headline liomacs/org-email-file "Emails")
         "* TODO [#A] Reply: %a :@home:@school:@work" :immediate-finish t)
	("l" "link" entry (file liomacs/org-inbox-file)
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
	("c" "org-protocol-capture" entry (file liomacs/org-inbox-file)
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))))


(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))) ;; ignore headlines with ignore tag

(provide 'liomacs-org)


