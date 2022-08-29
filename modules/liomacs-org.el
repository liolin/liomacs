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

(use-package org
  :hook
  (org-agenda-finalize . liomacs/org-agenda-finalize-hook)
  :bind
  ("C-c l" . org-store-link)
  ("C-c X" . org-capture)
  :config
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (require 'org-protocol)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (liomacs/load-org-modules)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ▾")
  (org-todo-keywords '((sequence "TODO" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-modules '(org-habit))
  (org-startup-folded t)
  (org-clock-sound "~/code/liomacs/ping.wav")
  (org-capture-templates
      `(("i" "inbox" entry (file liomacs/org-inbox-file)
         "* TODO %?")
	("e" "email" entry (file+headline liomacs/org-email-file "Emails")
         "* TODO [#A] Reply: %a :@home:@school:@work" :immediate-finish t)
	("l" "link" entry (file liomacs/org-inbox-file)
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
	("c" "org-protocol-capture" entry (file liomacs/org-inbox-file)
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))))


(provide 'liomacs-org)

