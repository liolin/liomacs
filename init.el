;;; -*- lexical-binding: t -*-

;; elpaca Setup
(defvar elpaca-installer-version 0.9)
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

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; custom-file and secrets
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq secret-file (expand-file-name "secrets.el" user-emacs-directory))
(load-file custom-file)
(require 'secrets secret-file t)

(setq-default liomacs/font "JetBrainsMono 10")

;; emacs setup
(use-package emacs
  :ensure nil
  ;; TODO: Fix this, gc-cons-threshold is not set back to old value
  ;; :hook
  ;; (minibuffer-setup . (lambda() (setq gc-cons-threshold most-positive-fixnum)))
  ;; (minibuffer-exit . (lambda() (setq gc-cons-threshold (* 8 1024 1024))))
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (show-trailing-whitespace t)
  :config
  (setq-default indent-tabs-mode nil)
  (setq ring-bell-function #'ignore
        load-prefer-newer t
        backup-by-copying t
        backup-directory-alist '(("." . "~/.saves"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        display-line-numbers-type 'visual
        read-process-output-max (* 10 1024 1024) ;; 10MB
        gc-cons-threshold 200000000)
  (set-frame-font liomacs/font nil t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0)

  (electric-pair-mode 1)
  (global-display-line-numbers-mode t)
  (dolist (mode '(term-mode-hook
		  shell-mode-hook
		  eshell-mode-hook
		  doc-view-mode-hook
		  pdf-view-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (set-fringe-mode 10))

;; evil
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
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :after evil
  :custom
  (undo-tree-auto-save-history)
  :config
  (global-undo-tree-mode))

(use-package gnuplot)
(use-package gnuplot-mode)

;; org
(setq liomacs/org-inbox-file "~/org/Agenda/inbox.org")
(setq liomacs/org-email-file "~/org/Agenda/emails.org")
(setq liomacs/org-agenda-todo-view
      '("i" "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning-days 7)))
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
		  ((org-agenda-overriding-header "III")
		   (org-agenda-files '("~/org/Agenda/work.org"))))
	 (todo "WORKING"
	       ((org-agenda-overriding-header "In Progress")
		(org-agenda-files '("~/org/Agenda/projects.org"
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
        ("roam-pdf"
         :base-directory "~/roam/"
         :recursive t
         :publishing-function org-latex-publish-to-pdf
         :publishing-directory "~/code/roam_html/pdf"
         :with-toc nil
         :section-number nil)
	("roam" :components ("roam-org" "roam-attachment" "roam-css" "roam-pdf"))))

(use-package org
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
     (gnuplot . t)
     (python . t)))
  :bind
  ("C-c l" . org-store-link)
  ("C-c X" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c t" . org-timestamp-inactive)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ▾")
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-todo-keywords '((sequence "TODO" "NEXT" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-startup-folded t)
  (org-attach-use-inheritance t)
  (org-duration-format 'h:mm)
  (org-agenda-files
   (append (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-tags-column 120)
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
  (org-latex-src-block-backend 'listings)
  (org-latex-listings-options '(("numbers" "left")))

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

  (add-to-list 'org-structure-template-alist '("d" . "definition"))

  (org-indent-mode)
  (visual-line-mode 1)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (org-link-set-parameters
   "yt"
   :follow
   (lambda (path) (async-shell-command (format "mpv \"https://%s\"" path))))

  ;; agenda-view
  (add-to-list 'org-agenda-custom-commands liomacs/org-agenda-todo-view)

  ;; org-latex
  (add-to-list 'org-latex-listings-langs '(csharp "[Sharp]C"))
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "subcaption"))
  (add-to-list 'org-latex-classes
               '("no-article"
                 "\\documentclass{article}
\\usepackage[a4paper]{geometry}

% Language and encoding
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}

\\usepackage{amsmath}
\\usepackage{amsfonts}

% Page margins
\\geometry{
  a4paper,
  top=3.3cm,
  head=36pt,
  foot=23pt,
  left=2.4cm,
  right=2.4cm,
  bottom=2.3cm
}

% Colors
\\usepackage{xcolor}

% Compact lists
\\usepackage{paralist}
\\usepackage{enumitem}
\\setlist[description]{%
  font={\\bfseries\\rmfamily}, % set the label font
}
\\newcommand{\\bulletdescriptionlabel}[1]{%
  \\hspace\\labelsep
  \\normalfont
  \\textbullet\\ %
\\bfseries #1}
\\newlist{itemdescript}{description}{2}
\\setlist[itemdescript,1]{before=\\let\\makelabel\\bulletdescriptionlabel}
\\setlist[itemdescript,2]{before=\\let\\makelabel\\bulletdescriptionlabel}
\\setlist{noitemsep}

% Header and footer
\\usepackage[automark,headsepline]{scrlayer-scrpage}
\\usepackage{scrhack} % removes warning about deprecated \"chapter\" command.

\\usepackage{titling}
\\clearpairofpagestyles
\\lohead{\\sffamily\\textbf{\\thetitle}}
\\rohead{\\thedate}
\\lofoot{\\sffamily \\theauthor}
\\cofoot{\\sffamily\\leftmark}
\\rofoot{\\sffamily\\thepage}
\\pagestyle{scrheadings}
% \\renewcommand*{\\chapterpagestyle}{scrheadings}

% Hyperlinks
\\usepackage{hyperref}
\\usepackage{soul}
% \\usepackage[anythingbreaks]{breakurl}
\\usepackage{etoolbox}

\\urlstyle{rm}
\\definecolor{linkcolor}{HTML}{81245D}
\\hypersetup{
  colorlinks,
  linkcolor={linkcolor},
  citecolor={linkcolor},
  urlcolor={linkcolor}
}

% Icons
\\usepackage{fontawesome}

% Theorem, Proofs, definitions, ...
\\usepackage{amsthm}
\\newtheorem{theorem}{Theorem}
\\theoremstyle{definition}
\\newtheorem{definition}{Definition}

% Boxes
\\usepackage[most]{tcolorbox}
\\definecolor{infobar}{HTML}{0085cd}
\\definecolor{infobackground}{HTML}{5fbfed}
\\newenvironment{info}[1][Info]
{
  \\begin{tcolorbox}[
      arc = 2mm,
      boxrule = 0pt,
      breakable,
      before skip=11pt,
      before skip=11pt,
      title = #1,
      fonttitle = \\sffamily\\bfseries,
      coltitle = white,
      colbacktitle = infobar,
      colback = infobackground,
      toptitle=2mm,
      bottomtitle=2mm,
      top=4mm,
      bottom=4mm
    ]
  }
  {
  \\end{tcolorbox}
}

\\definecolor{warnbar}{HTML}{c32e15}
\\definecolor{warnbackground}{HTML}{f39a8b}
\\newenvironment{warn}[1][Warning]
{
  \\begin{tcolorbox}[
      arc = 2mm,
      boxrule = 0pt,
      breakable,
      before skip=11pt,
      before skip=11pt,
      title = #1,
      fonttitle = \\sffamily\\bfseries,
      coltitle = white,
      colbacktitle = warnbar,
      colback = warnbackground,
      toptitle=2mm,
      bottomtitle=2mm,
      top=4mm,
      bottom=4mm
    ]
  }
  {
  \\end{tcolorbox}
}

% Code
\\usepackage{soul}
\\usepackage{listings}
\\usepackage{textcomp}
\\usepackage{dirtree}

\\definecolor{strings}{HTML}{448c25}
\\definecolor{comments}{HTML}{aaaaaa}
\\definecolor{keywords}{HTML}{aa3d8c}
\\definecolor{ndkeywords}{rgb}{.612,.36,.15}
\\definecolor{background}{HTML}{f4f4f4}
\\definecolor{numbers}{HTML}{a884e0}

% Default style
\\lstdefinestyle{default}{
  backgroundcolor=\\color{background},
  basicstyle=\\ttfamily\\small,
  breakatwhitespace=true,
  breaklines=true,
  commentstyle=\\color{comments}\\ttfamily,
  deletekeywords={},
  escapeinside={}{},
  extendedchars=true,
  frame=lines,
  keepspaces=true,
  identifierstyle=\\color{black},
  keywordstyle=\\color{keywords}\\bfseries,
  ndkeywordstyle=\\color{ndkeywords}\\bfseries,
  morekeywords={},
  numbers=left,
  numberstyle=\\ttfamily\\color{numbers},
  rulecolor=\\color{numbers},
  showspaces=false,
  showstringspaces=false,
  showtabs=false,
  stepnumber=1,
  stringstyle=\\color{strings}\\ttfamily,
  tabsize=2,
}
\\lstset{
  style=default,
  columns=fullflexible,
  upquote=true
}

\\lstdefinelanguage{javascript}{
  keywords={typeof, new, true, false, catch, function, return, null, catch, switch, var, if, in, while, do, else, case, break, const},
  ndkeywords={class, export, boolean, throw, implements, import, this},
  sensitive=false,
  comment=[l]{//},
  morecomment=[s]{/*}{*/},
  morestring=[b]',
  morestring=[b]\"
}

\\lstdefinelanguage{yaml}{
  ndkeywords={this},
  sensitive=false,
  comment=[l]{\\#},
}
\\lstdefinelanguage{json}{
  keywords={{,},:, string, number},
  sensitive=false,
  comment=[l]{\\#},
  morestring=[b]\"
}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*a{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("ost-summary"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\")))
  (add-to-list 'org-latex-classes
	       '("ost-exam-summary"
		 "\\documentclass{extarticle}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\")))
  (add-to-list 'org-latex-classes
	       '("personal-report"
		 "\\documentclass{article}"
		 ("\\subparagraph{%s} \\" . "\\subparagraph*{%s} \\")))
  (add-to-list 'org-latex-classes
	       '("acmart"
		 "\\documentclass{acmart}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package evil-org
  ;; TODO: this demand required?
  :demand t
  :after evil org
  :hook
  (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-ref
  :bind
  (:map org-mode-map
	("C-c ]" . org-ref-insert-link-hydra/body))
  :config
  (setq org-latex-prefer-user-labels t))

(defun liomacs/update-org-id-files ()
  "Adds all IDs from the org-roam files to the org-id-locations-file"
  (interactive)
  (let ((fil (org-roam-list-files)))
    (org-id-update-id-locations fil)))

(defun liomacs/search-roam ()
  (interactive)
  (rg (read-from-minibuffer "Pattern:") "*.org" "~/roam")
  (switch-to-buffer (rg-buffer-name)))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :hook
  ;; TODO: Enable again when everything is fine
  (org-roam-mode . lsp-deferred)
  :custom
  (org-roam-directory "~/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "\n- tags :: %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "pattern" plain "\n- tags :: %?\n\n\nWhen to use:\n- \n\n\nBenefits:\n- \n\n\nCosts:\n- "
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
   ("C-c n u" . liomacs/update-org-id-files)
   ("C-c n s" . liomacs/search-roam))
  :config
  (require 'org-roam-export)
  (add-hook 'org-export-before-processing-hook 'liomacs/ox-html-add-extra-sections)
  (add-hook 'org-export-before-processing-hook 'liomacs/ox-latex-add-conf)
  (org-roam-db-autosync-enable))

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

(defun liomacs/ox-html-add-extra-sections (backend)
  (when (and (eq backend 'html)
             (org-roam-node-at-point))
    (save-excursion
      (goto-char (point-max))
      (insert "\n* Backlinks")
      (liomacs/collect-backlinks-string backend))))

(defun liomacs/--remove-first-property-drawer ()
  (delete-region
   (point-min)
   (car
    (org-element-map
        (org-element-parse-buffer)
        'property-drawer #'org-element-end))))

(defun liomacs/--end-location-first-property-drawer ()
  (goto-char (point-min))
  (car
   (org-element-map
       (org-element-parse-buffer)
       'property-drawer #'org-element-end)))

(defun liomacs/ox-latex-add-conf (backend)
  (when (and (org-roam-node-at-point)
             (eq backend 'latex))
    (save-excursion
      (goto-char (liomacs/--end-location-first-property-drawer))
      (insert "#+INCLUDE: ./setup.conf\n"))))

(use-package org-noter
  :after (:any org pdf-view)
  :custom
  (org-noter-notes-window-location 'horizontal-split)
  (org-noter-always-crate-frame nil)
  (org-noter-hide-other t))

(use-package org-cliplink)

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; TODO: Improve this config
;; (use-package citar
;;   :after all-the-icons
;;   :custom
;;   (citar-bibliography org-cite-global-bibliography)
;;   :bind
;;   (("C-c w c o" . citar-open)
;;    (:map org-mode-map
;; 	 :package org
;; 	 ("C-c w C" . #'org-cite-insert))))

;; Comment out, because of debugging and i'm not really using it
;; (use-package org-alert
;;  :ensure t
;;  :after org
;;  :custom
;;  (alert-default-style 'libnotify)
;;  :config
;;  (progn
;;    (setq org-alert-interval 300)
;;    (setq org-alert-notify-cutoff 10)
;;    (setq org-alert-notify-after-event-cutoff 10)
;;    (org-alert-enable)))

;; completion
(use-package which-key
  :custom
  (which-key-idel-delay 1)
  :config
  (which-key-mode))

(use-package vertico
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
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ([remap imenu]  . consult-imenu))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark)

(use-package embark-consult)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-dealy 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-quit-at-boundary nil)
  (corfu-preselect-first t)

  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)

  (lsp-completion-provider :none) ; Use corfu instead the default for lsp completions

  :bind
  ("M-<tab>" . completion-at-point)
  (:map corfu-map
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("M-d" . corfu-info-documentation)
        ("M-l" . corfu-info-location))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  ;; https://kristofferbalintona.me/posts/202202270056/
  (defun liomacs/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'liomacs/corfu-enable-always-in-minibuffer 1)

  ;; Setup lsp to use corfu for lsp completion
  (defun liomacs/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package helpful
  :after embark
  :bind
  ([remap describe-function]  . helpful-callable)
  ([remap describe-command]   . helpful-command)
  ([remap describe-variable]  . helpful-variable)
  ([remap describe-key]       . helpful-key))



;; visual
(use-package ef-themes
  :config
  (load-theme 'ef-cherie t))
;; (load-theme 'base16-gruvbox-dark-hard-dark t)
;; (load-theme 'leuven-dark)

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

;; programming
(use-package direnv
  :config
  (direnv-mode))

(use-package cmake-mode)

(use-package fancy-compilation
  :config
  (fancy-compilation-mode))

(use-package restclient)

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ;; TODO: Add rust, haskell, ...
         )
  :preface
  (defun liomacs/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.20.2"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (nix . ("https://github.com/nix-community/tree-sitter-nix"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (java-mode . java-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (liomacs/setup-install-grammars)
  (use-package combobulate ;; based on tree sitter
    :ensure (:host github :repo "mickeynp/combobulate" :branch "master")
    :preface
    (setq combobulate-key-prefix "C-c o")
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (html-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode)))
  )

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package lsp-mode
  :diminish "LSP"
  ;; :hook (lsp-completion-mode . liomacs/corfu-setup-lsp)
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           java-ts-mode
           ) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil) ;; I use hs
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil) ;; W have `browse-url'
  (lsp-enable-on-type-formatting nil) ;; prettier and co handle this
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil) ;; This is treesitter's job
  ;; ui
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
  (lsp-inlay-hint-enable t)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil) ;; Related to highlighting, we use treesitter
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-completion
  :ensure nil
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package yasnippet
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  :init
  (yas-global-mode))

(use-package yasnippet-snippets)


(use-package lsp-ui
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance
                lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point)) ;; change back to 'bottom?

(use-package lsp-treemacs)

(use-package apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'latex-mode apheleia-mode-alist)
        'tex-fmt)
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist)
        'tex-fmt)
  (add-to-list 'apheleia-formatters '(tex-fmt "tex-fmt" "--stdin" "--nowrap"))
  (apheleia-global-mode +1))

;; TODO: Fix missing recipe
;; (use-package lsp-eslint
;;   :demand t
;;   :after lsp-mode)

(use-package editorconfig
  :init
  (editorconfig-mode 1))


;; mu4e
(defun liolin/mailto (url)
  (if (equal (length (s-trim url)) 0)
      (message-mail)
    (browse-url-mail url)))

(use-package mu4e
  :ensure nil
  :after org
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (mu4e-org-link-query-in-headers-mode t)
  (mu4e-change-filename-when-moving t)
  (mu4e-compose-format-flowed t)
  (mu4e-update-interval (* 1 60))
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
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  (setq mu4e-contexts
	(list
         (make-mu4e-context
	  :name "liolin"
	  :match-func
          (lambda (msg)
	    (when msg
	      (string-prefix-p "/liolin" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address     . "olivier.lischer@liolin.ch")
		  (user-full-name        . "Olivier Lischer")
                  (smtpmail-smtp-server  . "asmtp.mail.hostpoint.ch")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
		  (mu4e-drafts-folder    . "/liolin/Drafts")
		  (mu4e-sent-folder      . "/liolin/Sent")
		  (mu4e-trash-folder     . "/liolin/Trash")
		  (mu4e-refile-folder    . "/archiv")))
         ;; (make-mu4e-context
         ;;  :name "mailinglist"
         ;;  :match-func
         ;;  (lambda (msg)
         ;;    (when msg
         ;;      (string-prefix-p "/mailinglist" (mu4e-message-field msg :maildir))))
         ;;  :vars '((user-mail-address     . "liolin@liolin.ch")
         ;;          (user-full-name        . "Olivier Lischer")
         ;;          (smtpmail-smtp-server  . "asmtp.mail.hostpoint.ch")
         ;;          (smtpmail-smtp-service . 587)
         ;;          (smtpmail-stream-type  . 'starttls)
         ;;          (mu4e-drafts-folder    . "/mailinglist/Drafts")
         ;;          (mu4e-sent-folder      . "/mailinglist/Sent")
         ;;          (mu4e-trash-folder     . "/mailinglist/Trash")
         ;;          (mu4e-refile-folder    . "/archiv")))
	 (make-mu4e-context
	  :name "ost"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/ost" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address     . "olivier.lischer@ost.ch")
		  (user-full-name        . "Olivier Lischer")
                  (smtpmail-smtp-server  . "asmtp.mail.hostpoint.ch")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
		  (mu4e-drafts-folder    . "/ost/Drafts")
		  (mu4e-sent-folder      . "/ost/Sent")
		  (mu4e-trash-folder     . "/ost/Trash")
		  (mu4e-refile-folder    . "/archiv")))
	 (make-mu4e-context
	  :name "gmail"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address     . "olivier.lischer.blon@gmail.com")
		  (user-full-name        . "Olivier Lischer")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
		  (mu4e-drafts-folder    . "/gmail/[Gmail]/Entw&APw-rfe")
		  (mu4e-sent-folder      . "/gmail/[Gmail]/Gesendet")
		  (mu4e-trash-folder     . "/gmail/[Gmail]/Papierkorb")
		  (mu4e-refile-folder    . "/archiv")))))
  (add-to-list 'mu4e-bookmarks '(:name "overview" :query "flag:flagged OR flag:unread AND NOT flag:trashed" :key ?o))
  (add-to-list 'mu4e-bookmarks '(:name "notes" :query "maildir:/notes/* AND NOT flag:trashed" :key ?n)))

(use-package mu4e-alert
  :hook
  (elpaca-after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'libnotify))

(use-package smtpmail
  :ensure nil)

;; pdf-tool
(use-package pdf-tools
  :config
  (pdf-tools-install))

;; rss
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-db-directory "~/ownCloud/Private/shared/elfeeddb")
  ;;(elfeed-search-filter "@6-months-ago +unread")
  (elfeed-feeds '(("http://www.reddit.com/r/emacs/.rss" reddit emacs)  ;; Reddit /r/emacs
		  ("http://www.reddit.com/r/rust/.rss" reddit rust)    ;; Reddit /r/rust
		  ("https://events.ccc.de/feed/" cc)                   ;; CCC
		  ("https://blog.tecosaur.com/tmio/rss.xml" emacs org) ;; This Month in Org
		  ("https://xenodium.com/rss.xml" emacs)               ;; Some Tech & Emacs Blog
                  ("https://sachachua.com/blog/feed" emacs)            ;; Sasha Chua: Emacs Blog
                  ("https://os.phil-opp.com/rss.xml" rust os)          ;; Writing an OS in Rust
                  "https://thorstenball.com/atom.xml"                  ;; Thorsten Ball
                  "https://matklad.github.io/feed.xml"                 ;; matklad
                  ;; NZZ - News
                  ("https://www.nzz.ch/recent.rss" nzz news)           ;; NZZ Recent articels
                  ("https://www.nzz.ch/technologie.rss" nzz news)      ;; NZZ Technologie
                  )))
;; shell
(use-package eat)

;; dired
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
  :config
  (diredfl-global-mode))

(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package fd-dired)


;; ;; projectile
(setq liomacs/project-dir "~/code")
(use-package projectile
  :init
  (setq projectile-project-search-path (list liomacs/project-dir))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (setq
   projectile-globally-ignored-directories
   (mapcar (lambda (val) (concat (substring val 0 -1) "\\$"))
           projectile-globally-ignored-directories))
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-sytem 'default))

(use-package rg)

;; magit
(use-package transient)
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package smerge-mode
  :ensure nil
  :custom
  (smerge-command-prefix "\C-cv"))

;; code folding
(use-package hs-mode
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode))
;; (use-package origami-mode
;;   :ensure nil
;;   :hook
;;   (prog-mode . hs-minor-mode))


;; languages
(use-package rustic
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
  (rustic-mode . electric-pair-mode)
  :config
  (setq rust-mode-treesitter-derive t))

(use-package tuareg
  :if (file-exists-p "/home/liolin/.opam/default/share/emacs/site-lisp")
  :config
  (add-to-list 'load-path "/home/liolin/.opam/default/share/emacs/site-lisp")
  (require 'ocp-indent))

(use-package haskell-mode)
(use-package lsp-haskell
  :after haskell-mode
  :hook
  (haskell-mode . lsp))

(use-package agda2-mode
  :ensure nil
  :if (file-exists-p "/usr/share/agda/emacs-mode/")
  :load-path "/usr/share/agda/emacs-mode/")

;; (use-package plantuml-mode
;;   :custom
;;   (plantuml-executable-path "/usr/bin/plantuml")
;;   (plantuml-default-exec-mode 'executable)
;;   (plantuml-indent-level 4)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
;;   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

;; Arduino
(use-package arduino-mode
  :mode "\\.ino\\'")
(use-package arduino-cli-mode
  :hook arduino-mode
  :mode "\\.ino\\'"
  :custom
  (arduino-cli-default-fqbn "arduino:avr:uno")
  (arduino-cli-default-port "/dev/ttyACM0")
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))

;; Java
(use-package lsp-java
  :after lsp
  :config
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk")
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java"
        dap-java-java-command "/usr/lib/jvm/java-17-openjdk/bin/java"
        lsp-java-vmargs '("-Xmx4g")))

(use-package dap-mode
  :ensure t
  :after (lsp-mode dap-java)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

;; Python
(use-package pylsp
  :ensure nil
  :after lsp
  :hook (python-ts-mode . lsp)
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  (setq lsp-pyls-plugins-flake8-enabled t))

(use-package pyvenv
  :config
  (pyvenv-mode 1))


;; LaTeX
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package auctex
  :ensure (auctex
           :pre-build (("./autogen.sh")
                       ("./configure" "--without-texmf-dir" "--with-lispdir=.")
                       ("make")
                       ("install-info" "doc/auctex.info" "doc/dir")
                       ("install-info" "doc/preview-latex.info" "doc/dir")))
  :mode (("\\.tex\\'" . LaTeX-mode)
         ("\\.tex\\.erb\\'" . LaTex-mode)
         ("\\.etx\\'" . LaTex-mode))
  :init
  (add-hook 'tex-mode-hook
            (lambda ()
              (load "auctex.el")
              (setq TeX-command-extra-options "-shell-escape")))

  :hook
  (LaTeX-mode . lsp-deferred)
  (LaTeX-mode . flycheck-mode)
  (LaTeX-mode . turn-on-reftex)
  :config
  (setq-default TeX-global-PDF-mode 1)
  (setq-default  preview-scale-function 1.5)
  (setq-default Tex-master nil)
  (setq-default Tex-output-dir "out")
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-auctex 1
        reftex-default-bibliography '("~/biblio/main.bib")
        reftex-plug-into-AUCTeX t
        default-truncate-lines t
        TeX-save-query nil
        TeX-source-correlate-method 'synctex))

(use-package darkroom
  :hook
  (darkroom-mode . visual-line-mode))

;; (use-package json-mode
;;   :custom
;;   (js-indent-level 2))

(use-package yaml-mode)

(use-package lsp-ltex
  :after lsp
  :hook
  (text-mode . (lambda ()
                 (require 'lsp-ltex)
                 (lsp)))
  :init
  (setq lsp-ltex-version "15.2.0")
  :config
  (flycheck-add-next-checker 'lsp 'proselint)
  ;; (flycheck-add-next-checker 'proselint)
  (setq lsp-ltex-language "en-GB"))

(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_GB")
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB")
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))


;; ledger
(use-package ledger-mode
  :custom
  (ledger-mode-should-check-version nil)
  (ledger-mode-links-in-register nil)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-binary-path "hledger")
  (ledger-post-amount-alignment-column 65)
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode)))

;;
;; server
;;
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))
