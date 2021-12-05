(let ((backup-dir (expand-file-name "saves" liomacs/save-dir)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir)))

(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(provide 'liomacs-backup)
