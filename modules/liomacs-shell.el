(use-package better-shell
    :ensure t
    :bind (("C-'" . better-shell-shell)
           ("C-;" . better-shell-remote-open))
    :config
    (setq shell-file-name "fish"))

(provide 'liomacs-shell)
