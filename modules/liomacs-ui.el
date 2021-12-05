;;
;; Setup UI as I want
;;


(tool-bar-mode -1)    ;; Disable the tool bar
(menu-bar-mode -1)    ;; Disable the menu bar
(column-number-mode)  ;; Enables the column number in the mode line
(set-fringe-mode 10)  ;; Set the size of the fringe
(blink-cursor-mode 0) ;; Disable blinking cursor

(setq display-line-numbers-type 'relative)  ;; The line numbering should be realtive to current position
(global-display-line-numbers-mode t)  ;; Enables line numbering in all modes
;; Disable line numbering in the following modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'liomacs-ui)
