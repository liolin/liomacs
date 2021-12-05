;;
;; Starting point for my emacs configuration
;;


;; Move to own module
(defvar liomacs/full-name
  "Olivier Lischer")
(defvar liomacs/mail-address
  "olivier.lischer@liolin.ch")
(setq user-full-name liomacs/full-name
      user-mail-address liomacs/mail-address)



(defvar liomacs/emacs-dir
  "~/code/liomacs"
  "Top level Emacs dir.")
(defvar liomacs/module-dir
  (expand-file-name "modules" liomacs/emacs-dir)
  "Location of the custom configuration")
(defvar liomacs/save-dir
  (expand-file-name "cache" liomacs/emacs-dir)
  "Common place to save Emacs save/history-files.")
(defvar liomacs/modules
  '(liomacs-ui
    liomacs-server)
  "A list of all modules to load")


(unless (file-exists-p liomacs/save-dir)
  (make-directory liomacs/save-dir))
(add-to-list 'load-path liomacs/module-dir)

(mapc 'require liomacs/modules)

;; End init.el
