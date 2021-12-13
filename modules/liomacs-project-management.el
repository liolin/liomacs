(defvar liomacs/project-dir "~/code")

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
  (projectile-completion-system 'default)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p liomacs/project-dir)
    (setq projectile-project-search-path (list liomacs/project-dir)))
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'liomacs-project-management)
