;;
;; Setting up the GC
;;


(defun liomacs/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun liomacs/minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 8 1024 1024)))

(add-hook 'minibuffer-setup-hook #'liomacs/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'liomacs/minibuffer-exit-hook)

(provide 'liomacs-gc)
