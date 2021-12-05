;;
;; Setup straight for package management
;;


(defvar bootstrap-version)

(setq straight-base-dir liomacs/vendor-dir)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" liomacs/vendor-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(provide 'liomacs-package-management)
