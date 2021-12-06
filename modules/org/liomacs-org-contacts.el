;;
;; inspirede by: https://isamert.net/2021/04/21/managing-your-contacts-in-org-mode-and-syncing-them-to-your-phone-android-ios-whatever-.html
;;

(defun liomacs/build-property (template-string contact-property)
  (if-let ((value (org-entry-get nil contact-property)))
      (concat (format template-string value) "\n")
    ""))

(defun liomacs/build-date-property (template-string contact-property)
  (if-let ((value (org-entry-get nil contact-property)))
      (concat (format template-string (substring value 1 11)) "\n")
    ""))

(defun liomacs/build-name-property (template-string contact-property)
  (if-let ((value (split-string (org-entry-get nil contact-property))))
      (if (> (length value) 2)
	  (concat (format template-string (format "%s;%s" (pop value ) (string-join value " "))) "\n")
	(if (= (length value) 1)
	    (concat (format template-string (format "%s" (nth 0 value))) "\n")
	(concat (format template-string (format "%s;%s" (nth 1 value) (nth 0 value))) "\n")))
    ""))

(defun liomacs/export-to-vcard ()
  "Exports a org file to a vcf file"
  (interactive)
  (write-region
   (string-join
    (org-map-entries
     (lambda ()
       (string-join
	`("BEGIN:VCARD\nVERSION:3.0\n"
	  ,(build-property "FN:%s" "ITEM")
	  ,(if (not (string= (org-entry-get nil "GROUP") "Company"))
	       (build-name-property "N:%s" "ITEM") "")
	  ,(build-date-property "BDAY:%s" "BIRTHDAY")
	  ,(build-property "TITLE:%s" "TITLE")
	  ,(build-property "EMAIL;TYPE=INTERNET,HOME:%s" "EMAIL-P")
	  ,(build-property "EMAIL;TYPE=INTERNET,WORK:%s" "EMAIL-W")
	  ,(build-property "TEL;TYPE=CELL,VOICE:%s" "PHONE-M")
	  ,(build-property "TEL;TYPE=WORK,VOICE:%s" "PHONE-W")
	  ,(build-property "TEL;TYPE=HOME,VOICE:%s" "PHONE-H")
	  ,(build-property "ADR:%s" "LOCAT")
	  ,(build-date-property "ANNIVERSARY:%s" "ANNIVERSARY")
	  ,(build-property "NOTE:%s" "NOTE")
	  ,(format "REV:%s\n" (format-time-string "%Y-%m-%dT%T"))
	  "END:VCARD"))))
    "\n")
   nil
   (read-file-name
    "Where to save the .vcf?"
    "~/Downloads/"
   "contacts.vcf")))
