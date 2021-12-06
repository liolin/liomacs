;;
;; Starting point for my emacs configuration
;;


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
(defvar liomacs/vendor-dir
  (expand-file-name "vendor" liomacs/emacs-dir)
  "lisp from somewhere")
(defvar liomacs/save-dir
  (expand-file-name "cache" liomacs/emacs-dir)
  "Common place to save Emacs save/history-files.")
(defvar liomacs/modules
  '(liomacs-gc
    liomacs-ui
    liomacs-evil
    liomacs-completion
    liomacs-undo
    liomacs-help
    liomacs-doc
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
    liomacs-cpp

    ;;liomacs-mu4e
    liomacs-server)
  "A list of all modules to load")


(setq user-full-name liomacs/full-name
      user-mail-address liomacs/mail-address)

(unless (file-exists-p liomacs/save-dir)
  (make-directory liomacs/save-dir))
(add-to-list 'load-path liomacs/module-dir)
(add-to-list 'load-path liomacs/vendor-dir)

(require 'liomacs-package-management)
(mapc 'require liomacs/modules)


;; End init.el
