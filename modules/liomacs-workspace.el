;; persp-mode is a fork of the perspective-el which shares workspaces over frames
;; there are discussions for a merge of this two 

;; persp-mode: https://github.com/Bad-ptr/persp-mode.el
;; perspective-el: https://github.com/nex3/perspective-el
(use-package persp-mode
  :custom
  (persp-keymap-prefix (kbd "C-x x"))
  (persp-auto-save-opt 0) ;; no auto save of all perspective
  :bind ([remap persp-frame-switch] . persp-switch)
  :init
  (persp-mode))

(provide 'liomacs-workspace)
