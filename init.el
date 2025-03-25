;;; init.el --- liomacs  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; -------------------- GENERAL EMACS CONFIG
(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("M-j" . duplicate-dwim)
   ("M-g r" . recentf)
   ("M-s g" . grep)
   ("M-s f" . find-name-dired)
   ("C-x C-b" . ibuffer)
   ("RET" . newline-and-indent)
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil))
  :custom
  (ad-redefinition-action 'accept)
  (column-number-mode nil) ;; maybe enable?
  (line-number-mode nil)
  (completion-ignore-case t)
  (completions-detailed t)
  (completions-format 'one-column)
  (delete-by-moving-to-trash t)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (delete-selection-mode 1)
  (enable-recursive minibuffers t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (help-window-select t)
  (history-length 300)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_GB")
  (kill-do-not-save-duplicates t)
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (read-answer-short t)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (resize-mini-windows 'grow-only)
  (ring-bell-function #'ignore)
  (savehist-save-minibuffer-history t)    ; t is default
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
	 register-alist                       ; macros
	 mark-ring global-mark-ring           ; marks
	 search-ring regexp-search-ring))     ; searches
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  (set-mark-command-repeat-pop t) ; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
  (split-width-threshold 170)     ; TODO: Do I want this: So vertical splits are preferred
  (split-height-threshold nil)    ; TODO: Do I want this
  (shr-use-colors nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints nil)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers nil)
  (visible-bell nil)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  (show-trailing-whitespace t)

  :config
  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 105)

  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Set line-number-mode with relative numbering
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
			   (list "d"
					 (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
					 "show diff between the buffer and its file"))

  ;; On Terminal: changes the vertical separator to a full vertical line
  ;;              and truncation symbol to a right arrow
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)

  ;; Runs 'private.el' after Emacs inits
  (add-hook 'after-init-hook
			(lambda ()
			  (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
				(when (file-exists-p private-file)
				  (load private-file)))))

  ;; Add MELPA
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  :init
  (set-window-margins (selected-window) 2 0)

  (toggle-frame-maximized)
  (select-frame-set-input-focus (selected-frame))
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one

  (with-current-buffer (get-buffer-create "*scratch*")
	(insert (format ";;
;; Loading time : %s
;; Packages     : %s
;;
"
					(emacs-init-time)
					(number-to-string (length package-activated-list))))))

(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources
		(list (expand-file-name ".authinfo.gpg" user-emacs-directory)))
  (setq user-full-name "Olivier Lischer"
		user-mail-address "olivier.lischer@liolin.ch")

  ;; Use `pass` as an auth-source
  (when (file-exists-p "~/.password-store")
	(auth-source-pass-enable)))

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
	 ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 0))
	 ("\\*\\([Hh]elp\\)\\*"
	  (display-buffer-in-side-window)
	  (window-width . 75)
	  (side . right)
	  (slot . 0))
	 ("\\*\\(Ibuffer\\)\\*"
	  (display-buffer-in-side-window)
	  (window-width . 100)
	  (side . right)
	  (slot . 1))
	 ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 1))
	 ("\\*\\(grep\\|find\\)\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 2))
	 )))

;;; ICOMPLETE
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
			  ("C-n" . icomplete-forward-completions)
			  ("C-p" . icomplete-backward-completions)
			  ("C-v" . icomplete-vertical-toggle)
			  ("RET" . icomplete-force-complete-and-exit)
			  ("C-j" . exit-minibuffer)) ;; So we can exit commands like `multi-file-replace-regexp-as-diff'
  :hook
  (after-init . (lambda ()
				  (fido-mode -1)
				  (icomplete-vertical-mode 1)))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
			  :after #'minibuffer-hide-completions))


(use-package dired
  :ensure nil
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
	 ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
	 (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-al --group-directories-first")
  :init
  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
	"Creates *Dired-Side* like an IDE side explorer"
	(interactive)
	(add-hook 'dired-mode-hook 'dired-hide-details-mode)

	(let ((dir (if directory-path
				   (dired-noselect directory-path)
				 (if (eq (vc-root-dir) nil)
					 (dired-noselect default-directory)
				   (dired-noselect (vc-root-dir))))))

	  (display-buffer-in-side-window
	   dir `((side . left)
			 (slot . 0)
			 (window-width . 30)
			 (window-parameters . ((no-other-window . t)
								   (no-delete-other-windows . t)
								   (mode-line-format . (" "
														"%b"))))))
	  (with-current-buffer dir
		(let ((window (get-buffer-window dir)))
		  (when window
			(select-window window)
			(rename-buffer "*Dired-Side*")
			)))))

  (defun emacs-solo/window-dired-open-directory ()
	"Open the current directory in *Dired-Side* side window."
	(interactive)
	(emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (eval-after-load 'dired
	'(progn
	   (define-key dired-mode-map (kbd "G") 'emacs-solo/window-dired-open-directory))))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")

  (defun isearch-copy-selected-word ()
	"Copy the current `isearch` selection to the kill ring."
	(interactive)
	(when isearch-other-end
	  (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
		(kill-new selection)
		(isearch-exit))))

  ;; Bind `M-w` in isearch to copy the selected word, so M-s M-. M-w
  ;; does a great job of 'copying the current word under cursor'.
  (define-key isearch-mode-map (kbd "M-w") 'isearch-copy-selected-word))

(use-package smerge-mode
  :ensure nil
  :custom
  (smerge-command-prefix "\C-cv"))

(use-package hs-mode
  :ensure nil
  :defer t
  :hook
  (prog-mode . hs-minor-mode))

(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-prefer-plaintext t)
  (jsonrpc-event-hook nil)
  (eglot-code-action-indications nil) ;; Emacs 31 -- annoying as hell
  :init
  (fset #'jsonrpc--log-event #'ignore)

  (defun emacs-solo/eglot-setup ()
	"Setup eglot mode with specific exclusions."
	(unless (eq major-mode 'emacs-lisp-mode)
	  (eglot-ensure)))

  (add-hook 'prog-mode-hook #'emacs-solo/eglot-setup)

  (with-eval-after-load 'eglot
	(add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
	(add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) "/usr/lib/rustup/bin/rust-analyzer"))
	(add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

  :bind (:map
		 eglot-mode-map
		 ("C-c l a" . eglot-code-actions)
		 ("C-c l o" . eglot-code-actions-organize-imports)
		 ("C-c l r" . eglot-rename)
		 ("C-c l f" . eglot-format)))

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
			  ("M-8" . flymake-goto-next-error)
			  ("M-7" . flymake-goto-prev-error)
			  ("C-c ! n" . flymake-goto-next-error)
			  ("C-c ! p" . flymake-goto-prev-error)
			  ("C-c ! l" . flymake-show-buffer-diagnostics)
			  ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
  :custom
  ;; (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)      ;; Alternatives: », E, W, i, !, ?)
	 (warning "?" compilation-warning)
	 (note "i" compilation-info)))
  :config
  ;; Define the toggle function
  (defun toggle-flymake-diagnostics-at-eol ()
	"Toggle the display of Flymake diagnostics at the end of the line
and restart Flymake to apply the changes."
	(interactive)
	(setq flymake-show-diagnostics-at-end-of-line
		  (not flymake-show-diagnostics-at-end-of-line))
	(flymake-mode -1) ;; Disable Flymake
	(flymake-mode 1)  ;; Re-enable Flymake
	(message "Flymake diagnostics at end of line: %s"
			 (if flymake-show-diagnostics-at-end-of-line
				 "Enabled" "Disabled"))))

(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup)
  ;; if we wanna remove this hook at any time, eval:
  ;; (remove-hook 'before-save-hook #'whitespace-cleanup)
  )

(use-package minibuffer
  :ensure nil
  :custom
  (completion-styles '(partial-completion flex initials))
  (completions-format 'vertical)
  (completion-ignore-case t)
  (completion-show-help t)
  (completion-auto-select nil) ;; only turn this on if not using icomplete
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :config
  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
		'(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
  (add-hook 'minibuffer-setup-hook
			(lambda () (setq truncate-lines t)))

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package elec-pair
  :ensure nil
  :defer t
  :hook (after-init . electric-pair-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t)) ;; show matches within window splits

(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/org")
  (org-startup-with-inline-images nil)
  (org-todo-keywords '((sequence "TODO" "NEXT" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-startup-folded t)
  (org-attach-use-inheritance t)
  (org-duration-format 'h:mm)

  ;; Org agenda settings
  (org-agenda-files
   (append (directory-files "~/org/Agenda" t ".org")))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-tags-column 120)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  :config
  (setq liomacs/org-inbox-file "~/org/Agenda/inbox.org")
  (setq
   ;; Edit settings
   org-auto-align-tags t
   ;; org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t)

  ;; Ellipsis styling
  (setq org-ellipsis " ▼ ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  ;; Org agenda
  (require 'org-agenda)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (add-to-list 'org-agenda-custom-commands
			   '("i" "Agenda"
				 ((agenda ""
						  ((org-agenda-span 'day)
						   (org-deadline-warning-days 7)))
				  (todo "WORKING"
						((org-agenda-overriding-header "In Progress")
						 (org-agenda-files '("~/org/Agenda/projects.org"
											 "~/org/Agenda/work.org"
											 "~/org/Agenda/school.org"
											 "~/org/Agenda/GTD.org"))))
				  (todo "NEXT"
						((org-agenda-overriding-header "Next")
						 (org-agenda-files '("~/org/Agenda/projects.org"
											 "~/org/Agenda/work.org"
											 "~/org/Agenda/school.org"
											 "~/org/Agenda/GTD.org"))))
				  (agenda ""
						  ((org-agenda-span 'week)
						   (org-deadline-warning-days 7)))

				  (alltodo ""
						   ((org-agenda-overriding-header "To Refile")
							(org-agenda-files '("~/org/Agenda/inbox.org"))))
				  (alltodo ""
						   ((org-agenda-overriding-header "All TODOs"))))))

  ;; Org capture
  (require 'org-capture)
  (define-key global-map (kbd "C-c x") 'org-capture)
  (setq org-capture-templates
		`(("i" "inbox" entry (file liomacs/org-inbox-file)
		   "* TODO %?")
		  ("e" "email" entry (file+headline liomacs/org-email-file "Emails")
		   "* TODO [#A] Reply: %a @home:@school:@work" :immediate-finish t)
		  ("l" "link" entry (file liomacs/org-inbox-file)
		   "* TODO %(org-cliplink-capture)" :immediate-finish t)
		  ("c" "org-protocol-capture" entry (file liomacs/org-inbox-file)
		   "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))


  ;; Org latex
  (require 'ox-latex)
  (setq org-latex-title-command ""
		org-latex-toc-command ""
		org-latex-listings 't
		org-latex-compiler "xelatex"
		org-latex-prefer-user-labels t
		org-latex-pdf-process
		'("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o -bibtex %f")
		org-latex-src-block-backend 'listings
		org-latex-listings-options '(("numbers" "left")))
  (add-to-list 'org-latex-listings-langs '(csharp "[Sharp]C"))
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
				 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
			   '("no-article"
				 "\\documentclass{article}
\\usepackage[a4paper]{geometry}

% Language and encoding
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}

\\usepackage{amsmath}
\\usepackage{amssymb}
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
				 )))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; WHICH-KEY
(use-package which-key
  :ensure nil
  :defer t
  :hook
  (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

;;; WEBJUMP
(use-package webjump
  :defer t
  :ensure nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
	 ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
	 ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
	 ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

(use-package emacs-solo-sudo-edit
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/sudo-edit (&optional arg)
	"Edit currently visited file as root.
				 With a prefix ARG prompt for a file to visit.
				 Will also prompt for a file to visit if current
				 buffer is not visiting a file."
	(interactive "P")
	(if (or arg (not buffer-file-name))
		(find-file (concat "/sudo:root@localhost:"
						   (completing-read "Find file(as root): ")))
	  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

;;; EMACS-SOLO-MODE-LINE
;;
;;  Customizations to the mode-line
;;
(use-package emacs-solo-mode-line
  :ensure nil
  :no-require t
  :defer t
  :init
  ;; Shorten big branches names
  (defun emacs-solo/shorten-vc-mode (vc)
	"Shorten VC string to at most 20 characters.
 Replacing `Git-' with a branch symbol."
	(let* ((vc (replace-regexp-in-string "^ Git[:-]" "  " vc))) ;; Options:   ᚠ ⎇
	  (if (> (length vc) 20)
		  (concat (substring vc 0 20) "…")
		vc)))

  ;; Formats Modeline
  (setq-default mode-line-format
				'("%e" "  "
				  ;; (:propertize " " display (raise +0.1)) ;; Top padding
				  ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
				  (:propertize "λ  " face font-lock-keyword-face)

				  (:propertize
				   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

				  mode-line-frame-identification
				  mode-line-buffer-identification
				  "   "
				  mode-line-position
				  mode-line-format-right-align
				  "  "
				  (project-mode-line project-mode-line-format)
				  "  "
				  (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
				  "  "
				  mode-line-modes
				  mode-line-misc-info
				  "  ")
				project-mode-line t
				mode-line-buffer-identification '(" %b")
				mode-line-position-column-line-format '(" %l:%c"))

  ;; Provides the Diminish functionality
  (defvar emacs-solo-hidden-minor-modes
	'(abbrev-mode
	  eldoc-mode
	  flyspell-mode
	  flymake-mode
	  smooth-scroll-mode
	  outline-minor-mode
	  which-key-mode
	  apheleia-mode
	  hs-minor-mode
	  evil-collection-unimpaired-mode))

  (defun emacs-solo/purge-minor-modes ()
	(interactive)
	(dolist (x emacs-solo-hidden-minor-modes nil)
	  (let ((trg (cdr (assoc x minor-mode-alist))))
		(when trg
		  (setcar trg "")))))

  (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes))

;;; -------------------- NON TREESITTER AREA

;;; -------------------- TREESITTER AREA
;;; TYPESCRIPT-TS-MODE
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

;;; TSX-TS-MODE
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

;;; RUST-TS-MODE
(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer 't
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))

;;; TOML-TS-MODE
(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;;; MARKDOWN-TS-MODE
(use-package markdown-ts-mode
  :ensure t
  :mode "\\.md\\'"
  :defer 't
  :config
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;;; YAML-TS-MODE
(use-package yaml-ts-mode
  :ensure yaml-ts-mode
  :mode "\\.yml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")))

;;; DOCKERFILE-TS-MODE
(use-package dockerfile-ts-mode
  :ensure dockerfile-ts-mode
  :mode "\\Dockerfile.*\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))


;;; SERVER
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

;;; FOREIGN PACKAGES

;; EVIL
(use-package evil
  :ensure t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package org-roam
  :ensure t
  :after org
  :hook
  ;; TODO: Enable again when everything is fine
  (org-roam-mode . lsp-deferred)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n u" . liomacs/update-org-id-files)
   ("C-c n s" . liomacs/search-roam))
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
  :config
  (defun liomacs/update-org-id-files ()
	"Adds all IDs from the org-roam files to the org-id-locations-file"
	(interactive)
	(let ((fil (org-roam-list-files)))
	  (org-id-update-id-locations fil)))

  (defun liomacs/search-roam ()
	(interactive)
	(grep (read-from-minibuffer "Pattern:") "*.org" "~/roam")
	(switch-to-buffer (grep-last-buffer)))

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

  (require 'org-roam-export)
  (add-hook 'org-export-before-processing-hook 'liomacs/ox-html-add-extra-sections)
  (add-hook 'org-export-before-processing-hook 'liomacs/ox-latex-add-conf)
  (org-roam-db-autosync-enable)

  :init
  (setq org-roam-v2-ack t))

(use-package org-cliplink
  :ensure t
  :defer t)

(use-package org-contrib
  :ensure t
  :defer t
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; PDF-TOOLS
(use-package pdf-tools
  :ensure t
  :defer
  :config
  (pdf-tools-install))

;; MAGIT
(use-package transient
  :ensure t
  :defer)

(use-package magit
  :ensure t
  :defer
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package apheleia
  :ensure t
  :defer t
  :hook (prog-mode . apheleia-mode)
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
  (add-to-list 'apheleia-formatters '(tex-fmt "tex-fmt" "--stdin" "--nowrap")))

(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

(use-package haskell-ts-mode
  :ensure t
  :mode "\\.hs\\'"
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")))

(use-package auctex
  :ensure t
  :defer t
  :mode (("\\.tex\\'" . LaTeX-mode)
		 ("\\.tex\\.erb\\'" . LaTex-mode)
		 ("\\.etx\\'" . LaTex-mode))
  :hook
  (LaTeX-mode . (lambda ()
				  (load "auctex.el")
				  (setq TeX-command-extra-options "-shell-escape")))
  (LaTeX-mode . flymake-mode)
  (LaTeX-mode . turn-on-reftex)
  :config
  (setq-default TeX-global-PDF-mode 1
				preview-scale-function 1.5
				Tex-master nil
				Tex-output-dir "out")
  (setq TeX-auto-save t
		TeX-parse-self t
		reftex-plug-into-auctex 1
		reftex-default-bibliography '("~/biblio/main.bib")
		reftex-plug-into-AUCTeX t
		default-truncate-lines t
		TeX-save-query nil
		TeX-source-correlate-method 'synctex))

;; (use-package f
;;   :ensure t)
;; (use-package eglot-ltex
;;   :ensure nil
;;   :after f
;;   :load-path "/tmp/eglot-ltex"
;;   :defer t
;;   :hook (text-mode . (lambda ()
;;					   (require 'eglot-ltex)
;;					   (eglot-ensure)))
;;   :init
;;   (setq eglot-ltex-server-path "/home/liolin/.local/ltex-ls-plus-18.4.0"
;;		eglot-ltex-communication-channel 'stdio))

(use-package mu4e
  :ensure nil
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
  :ensure t
  :defer t
  :hook
  (elpaca-after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'libnotify))

(use-package smtpmail
  :ensure t
  :defer t)

;; EF-THEMES
(use-package ef-themes
  :ensure t
  :defer t
  :init
  (load-theme 'ef-cherie t))

(provide 'init)
;;; init.el ends here
