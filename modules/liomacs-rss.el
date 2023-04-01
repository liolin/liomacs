(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-db-directory "~/ownCloud/Private/shared/elfeeddb")
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
		  "http://www.reddit.com/r/rust/.rss"
		  "https://events.ccc.de/feed/"
		  "https://blog.tecosaur.com/tmio/rss.xml"
		  "https://xenodium.com/rss.xml"))) 

(provide 'liomacs-rss)
