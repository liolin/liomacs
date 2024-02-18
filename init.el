(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"

                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(require 'secrets "/home/liolin/code/liomacs/secrets.el" t)

(defun liomacs/find-file-large-file-hook ()
  "Use this to disable modes, which make emacs slow in large files"
  (interactive)
  ;; (font-lock-mode -1) ;; in extrem cases, this might also help
  (display-line-numbers-mode 0))


(use-package emacs
  :ensure nil
  :hook
  (minibuffer-setup . (lambda() (setq gc-cons-threshold most-positive-fixnum)))
  (minibuffer-exit . (lambda() (setq gc-cons-threshold (* 8 1024 1024))))
  :config
  (setq-default indent-tabs-mode nil)
  (setq ring-bell-function #'ignore)
  (setq display-line-numbers-type 'visual)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (electric-pair-mode 1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10)
  (blink-cursor-mode 0)
  (global-display-line-numbers-mode t)
  (dolist (mode '(term-mode-hook
		  shell-mode-hook
		  eshell-mode-hook
		  doc-view-mode-hook
		  pdf-view-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   indent-tabs-mode nil))

(setq liomacs/org-inbox-file "~/org/Agenda/inbox.org")
(setq liomacs/org-email-file "~/org/Agenda/emails.org")
(setq liomacs/org-agenda-todo-view
      '("i" "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning-days 365)))
	 (alltodo ""
		  ((org-agenda-overriding-header "To Refile")
		   (org-agenda-files '("~/org/Agenda/inbox.org"))))
	 (alltodo ""
		  ((org-agenda-overriding-header "Emails")
		   (org-agenda-files '("~/org/Agenda/emails.org"))))
	 (alltodo ""
		  ((org-agenda-overriding-header "OST")
		   (org-agenda-files '("~/org/Agenda/school.org"))))
	 (alltodo ""
		  ((org-agenda-overriding-header "IFS")
		   (org-agenda-files '("~/org/Agenda/work.org"))))
	 (todo "WORKING"
	       ((org-agenda-overriding-header "In Progress")
		(org-agenda-files '("~/org/Agenda/ba.org"
				    "~/org/Agenda/projects.org"
				    "~/org/Agenda/work.org"
				    "~/org/Agenda/school.org"
				    "~/org/Agenda/GTD.org"))))
	 (alltodo ""
		  ((org-agenda-overriding-header "Projects")
		   (org-agenda-files '("~/org/Agenda/projects.org")))))))

(setq org-publish-project-alist
      '(("roam-org"
	 :base-directory "~/roam/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :publishing-directory "~/code/roam_html/"
	 :html-head "<link rel=\"stylesheet\" href=\"static/css/roam.css\" type=\"text/css\"/>"
	 :html-preamble "<div class=\"header\"><a href=\"https://dg.liolin.ch\" title=\"liolin's digital garden\">liolin's digital garden</a></div>"
	 :html-validation-link nil
	 :with-toc nil
	 :section-number nil
         :auto-sitemap t
	 :sitemap-filename "index.org")
	("roam-attachment"
	 :base-directory "~/roam/static/attachment/"
	 :base-extension "png\\|jpg\\|jpeg"
	 :recursive t
	 :publishing-function org-publish-attachment
	 :publishing-directory "~/code/roam_html/static/attachment/")
	("roam-css"
	 :base-directory "~/roam/static/css/"
	 :base-extension "css"
	 :recursive t
	 :publishing-function org-publish-attachment
	 :publishing-directory "~/code/roam_html/static/css/")
	("roam" :components ("roam-org" "roam-attachment" "roam-css"))))

(defun liomacs/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))

(require 'cl-lib)
(defun w-summary-type (values printf)
  (format
   (or printf "%s")
   (cl-reduce (lambda (res ele)
		(cond
		 ((equal ele "[ ]") "[ ]")
		 (t res)))
	      (take 7 (reverse values)) :initial-value "[X]")))

(use-package org
  :demand t
  :init
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (latex . t)
     (shell . t)
     (python . t)))
  ;; :hook
  ;; (org-mode . lsp-deferred)
  :bind
  ("C-c l" . org-store-link)
  ("C-c X" . org-capture)
  ("C-c a" . org-agenda)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ▾")
  (org-todo-keywords '((sequence "TODO" "NEXT" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-startup-folded t)
  (org-attach-use-inheritance t)
  (org-duration-format 'h:mm)
  (org-agenda-files
   (append (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-capture-templates
   `(("i" "inbox" entry (file liomacs/org-inbox-file)
      "* TODO %?")
     ("e" "email" entry (file+headline liomacs/org-email-file "Emails")
      "* TODO [#A] Reply: %a @home:@school:@work" :immediate-finish t)
     ("l" "link" entry (file liomacs/org-inbox-file)
      "* TODO %(org-cliplink-capture)" :immediate-finish t)
     ("c" "org-protocol-capture" entry (file liomacs/org-inbox-file)
      "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))
  ;; org-babel
  (org-confirm-babel-evaluate nil)
  ;; org-planuml
  (org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

  ;; org-latex
  (org-latex-title-command "")
  (org-latex-toc-command "")
  (org-latex-listings 't)
  (org-latex-compiler "xelatex")
  (org-latex-prefer-user-labels t)
  (org-latex-pdf-process
   '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o -bibtex %f"))
  (org-latex-src-block-backend 'minted)

  ;; org-cite
  (org-cite-global-bibliography
   (directory-files "~/biblio" t "^[A-Z|a-z|0-9].+.bib$"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)

  ;; TODO: project alist
  :config
  (require 'org-protocol)
  (require 'org-tempo)
  (require 'org-agenda)
  (require 'ox-latex)

  (org-indent-mode)
  (visual-line-mode 1)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (org-link-set-parameters
   "yt"
   :follow
   (lambda (path) (async-shell-command (format "mpv \"https://%s\"" path))))

  (setq org-columns-summary-types
	'(("W" . w-summary-type)))


  ;; agenda-view
  (add-to-list 'org-agenda-custom-commands liomacs/org-agenda-todo-view)

  ;; org-latex
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "subcaption"))
  (add-to-list 'org-latex-classes
	       '("ost-summary"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\")))
  (add-to-list 'org-latex-classes
	       '("ost-exam-summary"
		 "\\documentclass{extarticle}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\"))))

(use-package org-ref
  :bind
  (:map org-mode-map
	("C-c ]" . org-ref-insert-link-hydra/body))
  :config
  (setq org-latex-prefer-user-labels t))

(use-package org-caldav
  :demand t
  :config
  (setq org-caldav-url liomacs/org-caldav-url
        org-caldav-calendar-id liomacs/org-caldav-calendar-id
        org-caldav-inbox liomacs/org-caldav-inbox
        org-caldav-files liomacs/org-caldav-files
        org-icalendar-timezone liomacs/org-icalendar-timezone
        org-caldav-calendars liomacs/org-caldav-calendars))

(use-package org-alert
  :demand t
  :after org
  :custom
  (alert-default-style 'libnotify)
  :config
  (progn
    (setq org-alert-interval 300)
    (setq org-alert-notify-cutoff 10)
    (setq org-alert-notify-after-event-cutoff 10)
    (org-alert-enable)))

(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t))

(use-package biblio)

;; TODO: Improve this config
(use-package citar
  :demand t
  :after all-the-icons
  :custom
  (citar-bibliography org-cite-global-bibliography)
  :bind
  (("C-c w c o" . citar-open)
   (:map org-mode-map
	 :package org
	 ("C-c w C" . #'org-cite-insert))))


(defun liomacs/update-org-id-files ()
  "Adds all IDs from the org-roam files to the org-id-locations-file"
  (interactive)
  (let ((fil (org-roam--list-files org-roam-directory)))
    (org-id-update-id-locations fil)))

(use-package org-roam
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :hook
  ;; TODO: Enable again when everything is fine
  ;; (org-roam-mode . lsp-deferred)
  (org-roam-mode . (lambda () (org-roam-db-autosync-mode 1)))
  :custom
  (org-roam-directory "~/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "\n- tags :: %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template #("${title:75} ${file:*} ${tags:10}" 11 21 (face org-tag)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ("C-c n j" . org-roam-dailies-capture-today)
  ("C-c n u" . liomacs/update-org-id-files))
  :config
  (require 'org-roam-export)
  (add-hook 'org-export-before-processing-hook 'liomacs/add-extra-sections))

(defun liomacs/collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (goto-char (point-max))
    (insert "\nNotes that link to this note\n")
    (let* ((backlinks (org-roam-backlinks-get (org-roam-node-at-point))))
      (dolist (backlink backlinks)
        (let* ((source-node (org-roam-backlink-source-node backlink))
               (point (org-roam-backlink-point backlink)))
          (insert
           (format "- [[./%s][%s]]\n"
                   (file-name-nondirectory (org-roam-node-file source-node))
                   (org-roam-node-title source-node))))))))

(defun liomacs/add-extra-sections (backend)
  (when (eq backend 'html)
      (when (org-roam-node-at-point)
        (save-excursion
          (goto-char (point-max))
          (insert "\n* Backlinks")
          (liomacs/collect-backlinks-string backend)))))

(use-package org-noter
  :demand t)

(use-package org-cliplink
  :demand t)

(use-package org-contrib
  :demand t
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package evil-org
  :demand t
  :after evil
  :hook
  (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(load-theme 'base16-gruvbox-dark-hard-dark t)

;; (use-package zenburn-theme
;;   :demand t
;;   :config
;;   (load-theme 'zenburn t))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package all-the-icons
  :demand t)

 (use-package evil
   :demand t
   :custom
   (evil-want-integration t)
   (evil-want-keybinding nil)
   (evil-want-C-u-scroll t)
   (evil-want-C-i-jump t)
   :bind
   ("<escape>" . keyboard-escape-quite)
   :config
   (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

;; (use-package evil-surround
;;   :demand t
;;   :after evil
;;   :config
;;   (globa-evil-surround-mode 1))


(use-package rainbow-delimiters
  :demand t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :demand t
  :init
  (which-key-mode)
  :custom
  (which-key-idel-delay 1))

(use-package vertico
  :demand t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :config
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :demand t
  :bind
  ("C-s" . consult-line))

(use-package marginalia
  :demand t
  :init
  (marginalia-mode))

(use-package embark
  :demand t)

(use-package embark-consult
  :demand t)

(use-package company
  :demand t
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection)
	:map prog-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (custom-minimum-prefix-length 1)
  (custom-idle-delay 0.5))

(use-package company-box
  :demand t
  :hook
  (company-mode . company-box-mode))


(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(use-package typescript-mode
  ;; :after tree-sitter
  :hook
  (typescript-mode . lsp-deferred)
  ;; (typescriptreact-mode . lsp-deferred)
  :custom
  (typescript-indent-level 2)
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  ;; (define-derived-mode typescriptreact-mode typescript-mode
  ;;   "TypeScript TSX")

  ;; use our derived mode for tsx files
  ;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(use-package tsi
  :after tree-sitter
  :ensure (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))


(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history)
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package fixmee
  :demand t
  :config
  (global-fixmee-mode 1))

(use-package helpful
  :demand t
  :after embark
  :bind
  ([remap describe-function]  . helpful-callable)
  ([remap describe-command]   . helpful-command)
  ([remap describe-variable]  . helpful-variable)
  ([remap describe-key]       . helpful-key))


;; mu4e
;;
(defun liolin/mailto (url)
  (if (equal (length (s-trim url)) 0)
      (message-mail)
    (browse-url-mail url)))

(use-package mu4e
  :ensure nil
  :after org
  :custom
  (mu4e-org-link-query-in-headers-mode t)
  (mu4e-change-filename-when-moving t)
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/.mail")
  (mu4e-maildir-shortcuts '(("/liolin/Inbox" . ?i)
			    ("/liolin/Sent"  . ?s)
			    ("/liolin/Trash" . ?t)))
  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:maildir . 22)
     (:mailing-list . 10)
     (:from . 22)
     (:subject)))
  :config
  (setq mu4e-contexts
	`(,(make-mu4e-context
	  :name "liolin"
	  :match-func (lambda (msg)
			(when msg
			  (string-prefix-p "/liolin" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address  . "olivier.lischer@liolin.ch")
		  (user-full-name     . "Olivier Lischer")
		  (mu4e-drafts-folder . "/liolin/Drafts")
		  (mu4e-sent-folder   . "/liolin/Sent")
		  (mu4e-trash-folder  . "/liolin/Trash")
		  (mu4e-refile-folder . "/archiv")))
	 ,(make-mu4e-context
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
	 ,(make-mu4e-context
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
  (add-to-list 'mu4e-bookmarks '(:name "overview" :query "flag:flagged OR flag:unread AND NOT flag:trashed" :key ?o))
  (add-to-list 'mu4e-bookmarks '(:name "notes" :query "maildir:/notes/* AND NOT flag:trashed" :key ?n)))

(use-package mu4e-alert
  :demand t
  :hook
  (elpaca-after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'libnotify))

(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-smtp-server "asmtp.mail.hostpoint.ch")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (message-send-mail-function 'smtpmail-send-it))

;;
;;
;; pdf-tool
;;
(use-package pdf-tools
  :demand t
  :config
  (pdf-tools-install))

;;
;; rss
;;
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-db-directory "~/ownCloud/Private/shared/elfeeddb")
  (elfeed-search-filter "@1-day-ago +unread")
  (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"      ;; Reddit /r/emacs
		  "http://www.reddit.com/r/rust/.rss"       ;; Reddit /r/rust
		  "https://events.ccc.de/feed/"             ;; CCC
		  "https://blog.tecosaur.com/tmio/rss.xml"  ;; This Month in Org
		  "https://xenodium.com/rss.xml"            ;; Some Tech & Emacs Blog
                  "https://sachachua.com/blog/feed"         ;; Sasha Chua: Emacs Blog
                  "https://os.phil-opp.com/rss.xml"         ;; Writing an OS in Rust
                  ;; NZZ - New
                  "https://www.nzz.ch/recent.rss"           ;; NZZ Recent articels
                  "https://www.nzz.ch/international.rss"    ;; NZZ International
                  "https://www.nzz.ch/schweiz.rss"          ;; NZZ Switzerland
                  "https://www.nzz.ch/technologie.rss"      ;; NZZ Technologie
                  )))
;;
;; shell
;;
(use-package eat
  :demand t)

;;
;; dired
;;
(use-package dired
  :ensure nil
  :hook
  (dired-mode . (lambda ()
                  (define-key
                   evil-normal-state-local-map
                   (kbd "h") 'dired-up-directory)
                  (define-key
                   evil-normal-state-local-map
                   (kbd "l") 'dired-find-file)))
  :custom
  (dired-listing-switches "-alh"))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :demand t
  :config
  (diredfl-global-mode))

(use-package dired-rsync
  :demand t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package fd-dired
  :demand t)


;;
;; projectile
;;
(setq liomacs/project-dir "~/code")
(use-package projectile
  :demand t
  :init
  (when (file-directory-p liomacs/project-dir)
    (setq projectile-project-search-path (list liomacs/project-dir)))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-sytem 'default)
  :config
  (projectile-mode))

(use-package rg
  :demand t)

;;
;; magit
;;
(use-package magit
  :demand t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :demand t
  :hook
  (magit-pre-refresh diff-hl-magit-pre-refresh)
  (magit-post-refresh diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;;
;; lsp
;;
(use-package lsp-mode
  :demand t
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :demand t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; languages
(use-package rustic
  :demand t
  :bind
  (:map rustic-mode-map
	("C-c C-c j" . hs-show-block)
	("C-c C-c J" . hs-show-all)
	("C-c C-c k" . hs-hide-block)
	("C-c C-c K" . hs-hide-all)
	("C-c C-c i" . lsp-ui-imenu)
	("C-c C-c l" . flycheck-list-errors)
	("C-c C-c a" . lst-execute-code-action)
	("C-c C-c r" . lst-rename)
	("C-c C-c q" . lst-workspace-restart)
	("C-c C-c Q" . lst-workspace-shutdown)
	("C-c C-c s" . lst-rust-analyzer-status))
  :custom
  (rustic-format-display-method #'ignore)
  :hook
  (rustic-mode . lsp)
  (rustic-mode . hs-minor-mode)
  (rustic-mode . electric-pair-mode))

(use-package haskell-mode)
(use-package lsp-haskell
  :after haskell-mode
  :hook
  (haskell-mode . lsp))

(use-package plantuml-mode
  :custom
  (plantuml-executable-path "/usr/bin/plantuml")
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 4)
  :config
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

;;
;; Arduino
;;
(use-package arduino-mode
  :mode "\\.ino\\'")
(use-package arduino-cli-mode
  :ensure t
  :hook arduino-mode
  :mode "\\.ino\\'"
  :custom
  (arduino-cli-default-fqbn "arduino:avr:uno")
  (arduino-cli-default-port "/dev/ttyACM0")
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))

;;
;; Java
;;
(use-package lsp-java
  :demand t
  :hook
  (java-mode . lsp-deferred))

;;
;; LaTeX
;;
;; TODO: Is not found
;; (use-package auctex
;;   :demand t
;;   :hook
;;   (TeX-mode . lsp-deferred)
;;   (TeX-mode . flycheck-mode)
;;   (TeX-mode . turn-on-reftex)
;;   :config
;;   (setq TeX-auto-save t
;; 	TeX-parse-self t
;; 	reftex-plug-into-auctex 1
;; 	reftex-default-bibliography '("~/biblio/main.bib"))
;;   (setq-default Tex-master nil))

(use-package json-mode
  :demand t
  :custom
  (js-indent-level 2))

(use-package yaml-mode
  :demand t)

;;
;; flycheck
;;
(use-package flycheck
  :demand t)

(use-package lsp-ltex
  :demand t
  ;;:after lsp
  :init
  (setq lsp-ltex-version "15.2.0")
  :config
  (flycheck-add-next-checker 'lsp 'proselint)
  (setq lsp-ltex-language "en-GB"))

;;
;; ledger
;;
(use-package ledger-mode
  :demand t
  :hook
  (ledger-mode . company-mode)
  :custom
  (ledger-mode-should-check-version nil)
  (ledger-mode-links-in-register nil)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-binary-path "hledger")
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode)))

;;
;; server
;;
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))


;; Don't install anything. Defer execution of BODY
(elpaca nil (message "deferred"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/liolin/org/Agenda/Events.org" "/home/liolin/org/Agenda/GTD.org" "/home/liolin/org/Agenda/ba.org" "/home/liolin/org/Agenda/calendar_ost.org" "/home/liolin/org/Agenda/emails.org" "/home/liolin/org/Agenda/inbox.org" "/home/liolin/org/Agenda/projects.org" "/home/liolin/org/Agenda/reports.org" "/home/liolin/org/Agenda/sa.org" "/home/liolin/org/Agenda/school.org" "/home/liolin/org/Agenda/work.org") nil nil "Customized with use-package org")
 '(org-latex-src-block-backend 't nil nil "Customized with use-package org")
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (if
		 (y-or-n-p "Tangle?")
		 (org-babel-tangle)))
	   nil t)
     (projectile-project-compilation-cmd . "cd Documentation/ && make")
     (lsp-ltex-language . "de-CH")
     (org-hugo-base-dir . "~/code/dg"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
