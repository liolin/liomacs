;; Thanks to Sidharth Arya
;; https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
(defun liomacs/org-roam-export-all()
  (interactive)
  (dolist (fil (org-roam--list-files org-roam-directory))
    (with-current-buffer (find-file-noselect fil)
      (org-hugo-export-to-md)
      (kill-buffer))))

(defun liomacs/testing ()
  (interactive)
  (message "testing my testing")
  (let ((my_list (org-roam-db-query "SELECT file FROM nodes INNER JOIN tags ON nodes.id = tags.node_id WHERE tag = '\"private\"';")))
    (dolist (file (org-roam--list-files org-roam-directory))
      (if (member (list (expand-file-name file)) my_list)
	  (message "found")
	(with-current-buffer (find-file-noselect file)
	  (org-hugo-export-to-md)
	  (kill-buffer))))))

(defun liomacs/org-hugo--org-roam-save-buffer(&optional no-trace-links)
  "On save export to hugo"
  (when (org-roam-file-p)
    (org-hugo-export-to-md)))


(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  (org-roam-mode . flyspell-mode)
  ;;(after-save . liomacs/org-hugo--org-roam-save-buffer)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam/")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))



(use-package org-roam-ui
  ;; :straight
  ;; (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))
