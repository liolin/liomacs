(defun liomacs/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun liomacs/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  (:map elfeed-search-mode-map
	("q" . liolin/elfeed-save-db-and-bury)
	("Q" . liolin/elfeed-save-db-and-bury))
  :custom
  (elfeed-db-directory "~/ownCloud/Private/shared/elfeeddb")
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
		  "http://www.reddit.com/r/rust/.rss"
		  "https://events.ccc.de/feed/"
		  "https://blog.tecosaur.com/tmio/rss.xml"
		  "https://xenodium.com/rss.xml"))) 

(provide 'liomacs-rss)
