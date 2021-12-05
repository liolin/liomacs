;;
;; Setting up the GC
;;


(defvar liomacs/gc-cons-threshold (* 8 1024 1024))

(setq-default
 gc-cons-threshold liomacs/gc-cons-threshold)



(provide 'liomacs-gc)
