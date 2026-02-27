;;; clock-reports.el --- liomacs -*- lexical-binding: t; -*-
;;; Commentary:
;;;

;;; Code:

(defun liomacs/clock-org-clock-table-level (row)
  "Returns the level value from ROW."
  (car row))

(defun liomacs/clock-org-clock-table-headline (row)
  "Returns the headline value from ROW."
  (nth 1 row))

(defun liomacs/clock-org-clock-table-tags (row)
  "Returns the tags value from ROW."
  (nth 2 row))

(defun liomacs/clock-org-clock-table-timestamp (row)
  "Returns the timestamp value from ROW."
  (nth 3 row))

(defun liomacs/clock-org-clock-table-time-spent (row)
  "Returns the sum of all time spent from ROW."
  (nth 4 row))

(defun liomacs/clock-org-clock-table-properties (row)
  "Returns the properties value from ROW."
  (nth 5 row))

(defun liomacs/clock-calc-spent-time (table)
  "Calculates the spent time per level 2 headlines according to TABLE."
  (seq-filter
   (lambda (row)
     (> (liomacs/clock-org-clock-table-level row) 1))
   table))

(defun liomacs/clock-calc-table (start &optional end)
  "Returns the org-clock table with with tags between START and END."
  (nth 2 (org-clock-get-table-data
          buffer-file-name
          (list :tags t :maxlevel 2 :tstart start :tend end))))

(defun liomacs/clock-cleanup-table (table)
  "Removes not needed columns from the TABLE.
Only the headline (index 1) and time spent (index 4) are needed."
  (seq-map (lambda (row)
             (list (nth 1 row) (nth 4 row)))
           table))

(defun liomacs/clock-work-by-headline (start &optional end)
  "Get data between START and END."
  (let ((table (liomacs/clock-calc-table start end)))
    (liomacs/clock-cleanup-table (liomacs/clock-calc-spent-time table))))

(defun liomacs/clock-burn-down-data (start end total)
  "Get burn down data between START and END where TOTAL is the max."
  (seq-map
   (lambda (elt)
     (- total elt))
   (liomacs/--clock-scan (liomacs/clock-weekly-total-time start end) 0)))

(defun liomacs/--clock-scan (list total)
  "Builds a new list from LIST where the car element is the added to TOTAL.
In the resulting list the curent element will be the sum of the previous element and the current element."
  (if (eq (seq-length list) 1)
      (cons (+ total (car list)) nil)
    (let ((new-total (+ total (car list))))
      (cons new-total (liomacs/--clock-blub (cdr list) new-total)))))


(defun liomacs/clock-weekly-total-time (start end)
  "Returns a list of spent times per week between START and END.
START and END are strings and are parsible by `date-to-time'."
  (let* ((start-date (date-to-time start))
         (end-date    (date-to-time end)))
    (liomacs/--clock-weekly-total-time start-date end-date)))

(defun liomacs/--clock-weekly-total-time (start-date end-date)
  "Returns a list of spent times per week between START-DATE and END-DATE."
  (unless (time-less-p end-date start-date)
    (let ((new-start (liomacs/add-days-to-date start-date 7))
          (block-end (liomacs/add-days-to-date start-date 6)))
      (cons
       (liomacs/clock-org-clock-get-spent-time start-date end-date)
       (liomacs/clock-blub new-start end-date)))))


(defun liomacs/clock-org-clock-get-spent-time (start-date end-date)
  "Returns time spent between START-DATE and END-DATE."
  (let ((time-spent (liomacs/clock-org-clock-table-time-spent
                     (car (nth 2 (org-clock-get-table-data
                                  buffer-file-name
                                  (list :tags t
                                        :maxlevel 1
                                        :tstart (format-time-string "%Y-%m-%d" start-date)
                                        :tend (format-time-string "%Y-%m-%d" (liomacs/add-days-to-date start-date 6)))))))))
    (if (eq time-spent nil) 0 time-spent)))

(defun liomacs/add-days-to-date (start-date days)
  (let ((days-in-s (* days 24 60 60)))
    (time-add start-date days-in-s)))


(provide 'clock-reports)
;;; clock-reports.el ends here
