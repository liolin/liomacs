;;; dotfiles.el --- liomacs  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defcustom dotfiles/configuration-file nil
  "Path to the Org mode file containing the configuratio."
  :type 'file)

(defcustom dotfiles/package-manager "pacman"
  "Package manager to install required packages."
  :type 'string)

(defcustom dotfiles/package-manager-flags "-Syu"
  "Flags passed to the package manager to install packages."
  :type 'string)

(defcustom dotfiles/package-manager-with-sudo t
  "If set to t sudo is used to install the packages."
  :type 'boolean)

(defcustom dotfiles/sudo-command "sudo"
  "The command to to acquire super user rights."
  :type 'string)

(defun dotfiles/install-all-applications ()
  "Installs all configurations and packages.")

(defun dotfiles/install-application (application-name)
  "Installs the application with the name APPLICATION-NAME."
  (interactive "sApp: ")
  (dotfiles/install-application-packages application-name)
  (dotfiles/install-application-configuration application-name))

(defun dotfiles/install-application-configuration (application-name)
  "Installs only the configuration for the application with the name APPLICATION-NAME."
  (interactive "sApp: ")
  (with-current-buffer (find-file-noselect dotfiles/configuration-file t)
	(save-excursion
	  (goto-char (point-min))
	  (unless (re-search-forward (format "* %s" application-name))
		(user-error "Application '%s' not found in config file" application-name))
	  (org-narrow-to-subtree)
	  (org-babel-tangle)
	  (widen))))

(defun dotfiles/install-application-packages (application-name)
  "Installs only the required packages for the application with the name APPLICATION-NAME."
  (interactive "sApp: ")
  (with-current-buffer (find-file-noselect dotfiles/configuration-file t)
	(save-excursion
	  (goto-char (point-min))
	  (unless (re-search-forward (format "* %s" application-name))
		(user-error "Application '%s' not found in config file" application-name))
	  (org-narrow-to-subtree)
	  (re-search-forward "** Required Packages")
	  (re-search-forward "begin_example")
	  (widen)
	  (let ((node (org-element-at-point)))
		(unless (eq (org-element-type node) 'example-block)
		  (error "Not an example block"))
		(let ((packages (string-split (string-trim (string-replace "\n" " "(org-element-property :value node))))))
		  (if dotfiles/package-manager-with-sudo
			  (comint-run dotfiles/sudo-command (append (list dotfiles/package-manager dotfiles/package-manager-flags) packages))
			(comint-run dotfiles/package-manager (append (list dotfiles/package-manager-flags) packages))))))))

(provide 'dotfiles)
;;; dotfiles.el ends here
