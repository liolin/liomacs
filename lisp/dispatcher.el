;;; dispatcher.el --- liomacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This module provides functions to call Emacs Lisp functions from the CLI using emacsclient.

;;; Code:

(defvar liomacs/command-map '(("pass"    . liomacs/pass)
                              ("capture" . org-capture)))

(defun liomacs/dispatch-entry ()
  "The function to be called externall from Emacs.
For example using emacsclient."
  (call-interactively 'liomacs/dispatch)
  (delete-frame (selected-frame)))

(defun liomacs/dispatch (key)
  "Entry point for the whole dispatching stuff, where KEY is the action to be perfomred."
  (interactive
   (list (completing-read
          "Execute: "
          (liomacs/dispatch-prompt liomacs/command-map)
          nil
          'match)))
  (let ((entry (assoc key liomacs/command-map)))
    (if entry
        (funcall (cdr entry))
      (error "Not a valid key defined in `liomacs/command-map`:" key))))

(defun liomacs/dispatch-prompt (command-map)
  "Returns a list consisting of the keys from the assoc list COMMAND-MAP."
  (mapcar (lambda (element)
            (car element))
          command-map))


(defun liomacs/pass ()
  "Opens the pass buffer and asks the user for an entry."
  (progn
    (pass)
    (call-interactively 'pass-goto-entry)
    (pass-copy)))

(provide 'dispatcher)
;;; dispatcher.el ends here
