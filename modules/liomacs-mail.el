(use-package mu4e
  :config
  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "liolin"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/liolin" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "olivier.lischer@liolin.ch")
		  (user-full-name    . "Olivier Lischer")
		  (mu4e-drafts-folder . "/liolin/Drafts")
		  (mu4e-sent-folder . "/liolin/Sent")
		  (mu4e-trash-folder . "/liolin/Trash")
		  (mu4e-refile-folder . "/archiv")))

	 (make-mu4e-context
	  :name "notes"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/notes" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "notes@liolin.ch")
		  (user-full-name    . "Olivier Lischer")
		  (mu4e-drafts-folder . "/notes/Drafts")
		  (mu4e-sent-folder . "/notes/Sent")
		  (mu4e-trash-folder . "/notes/Trash")
		  (mu4e-refile-folder . "/archiv")))

	 (make-mu4e-context
	  :name "gmail"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "olivier.lischer.blon@gmail.com")
		  (user-full-name    . "Olivier Lischer")
		  (mu4e-drafts-folder . "/gmail/[Gmail]/Entw&APw-rfe")
		  (mu4e-sent-folder . "/gmail/[Gmail]/Gesendet")
		  (mu4e-trash-folder . "/gmail/[Gmail]/Papierkorb")
		  (mu4e-refile-folder . "/archiv")))))

  :custom
  (mu4e-org-link-query-in-headers-mode t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/.mail")


  (mu4e-maildir-shortcuts
   '(("/liolin/Inbox"  . ?i)
     ("/liolin/Sent"    . ?s)
     ("/liolin/Trash"   . ?t)))
  :config
  (add-to-list 'mu4e-view-actions '("Browser" . mu4e-action-view-in-browser) t))


(use-package mu4e-alert
  :hook
  (after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'libnotify))

(use-package smtpmail
  :custom
  (smtpmail-smtp-server "asmtp.mail.hostpoint.ch")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (message-send-mail-function 'smtpmail-send-it))
(provide 'liomacs-mail)
