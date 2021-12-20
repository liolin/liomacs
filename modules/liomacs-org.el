(defun liomacs/org-agenda-finalize-hook()
  (evil-normal-state))

(defun liomacs/load-org-modules ()
  (let ((org-modules-dir (expand-file-name "org" liomacs/module-dir)))
    (mapc 'load (directory-files org-modules-dir t "^[^#].*el$"))))


(use-package org
  :hook
  (org-agenda-finalize . liomacs/org-agenda-finalize-hook)
  :bind
  ("C-c l" . org-store-link)
  :config
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (liomacs/load-org-modules)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ▾")
  (org-todo-keywords '((sequence "TODO" "WORKING" "WAIT" "|" "DONE" "KILL")))
  (org-log-done 'time)
  (org-modules '(org-habit))
  (org-startup-folded t))


(provide 'liomacs-org)
