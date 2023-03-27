;;
;; Starting point for my emacs configuration
;;


(defgroup liomacs nil "Custom group for my custom settings")

(defvar liomacs/full-name
  "Olivier Lischer")
(defvar liomacs/mail-address
  "olivier.lischer@liolin.ch")

(defvar liomacs/emacs-dir
  "~/code/liomacs"
  "Top level Emacs dir.")
(defvar liomacs/module-dir
  (expand-file-name "modules" liomacs/emacs-dir)
  "Location of the custom configuration")
(defvar liomacs/org-modules-dir
  (expand-file-name "org" liomacs/module-dir)
  "Location of the custom org modules")
(defvar liomacs/vendor-dir
  (expand-file-name "vendor" liomacs/emacs-dir)
  "lisp from somewhere")
(defvar liomacs/save-dir
  (expand-file-name "cache" liomacs/emacs-dir)
  "Common place to save Emacs save/history-files.")
(defvar liomacs/share-dir
  "~/ownCloud/Private/shared/"
  "Location where I want to store synced files")

(defvar liomacs/modules
  '(liomacs-gc
    liomacs-ui
    liomacs-evil
    liomacs-completion
    liomacs-undo
    liomacs-help
    liomacs-doc
    liomacs-bookmark
    ;;liomacs-workspace
    liomacs-pass
    liomacs-rss
    liomacs-org
    liomacs-hledger
    liomacs-backup
    liomacs-project-management
    liomacs-version-control
    liomacs-lsp
    liomacs-diagnostics
    liomacs-snippets
    liomacs-debugging
    liomacs-rust
    liomacs-java
    liomacs-cpp
    liomacs-haskell
    liomacs-web
    liomacs-nix
    liomacs-csharp
    liomacs-kotlin
    liomacs-python
    liomacs-latex
    liomacs-yaml
    liomacs-editorconfig
    liomacs-shell
    liomacs-server)
  "A list of all modules to load")


(setq user-full-name liomacs/full-name
      user-mail-address liomacs/mail-address)

(let ((gc-cons-threshold most-positive-fixnum))
  (unless (file-exists-p liomacs/save-dir)
    (make-directory liomacs/save-dir))
  (add-to-list 'load-path liomacs/module-dir)
  (add-to-list 'load-path liomacs/vendor-dir)

  (require 'liomacs-package-management)
  (mapc 'require liomacs/modules)
  ;; (unless (equal (system-name) "NB-IFS-501047")
  ;;   (require 'liomacs-mail))


  ;; End init.el
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(safe-local-variable-values
     '((org-attach-id-dir . "static/attachment")
       (org-attach-id-dir . "static/pdf")
       (lsp-ltex-language . "de-CH")
       (eval add-hook 'after-save-hook
	     (lambda nil
	       (if
		   (y-or-n-p "Tangle?")
		   (org-babel-tangle)))
	     nil t)
       (org-attach-id-dir . "attachment/"))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/liolin/org/Agenda/Events.org" "/home/liolin/org/Agenda/GTD.org" "/home/liolin/org/Agenda/Habits.org" "/home/liolin/org/Agenda/ba.org" "/home/liolin/org/Agenda/calendar_ost.org" "/home/liolin/org/Agenda/emails.org" "/home/liolin/org/Agenda/inbox.org" "/home/liolin/org/Agenda/projects.org" "/home/liolin/org/Agenda/reports.org" "/home/liolin/org/Agenda/sa.org" "/home/liolin/org/Agenda/school.org" "/home/liolin/org/Agenda/work.org") nil nil "Customized with use-package org")
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "cd Documentation/ && make")
     (eval setq-local compile-command
	   (format "cd %s && make"
		   (concat
		    (locate-dominating-file default-directory ".dir-locals.el")
		    "Documentation")))
     (org-attach-id-dir . "static/attachment")
     (org-attach-id-dir . "static/pdf")
     (lsp-ltex-language . "de-CH")
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (if
		 (y-or-n-p "Tangle?")
		 (org-babel-tangle)))
	   nil t)
     (org-attach-id-dir . "attachment/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
