;;; purpose-utils.el --- Utilities -*- lexical-binding: t -*-

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:
;; Utilities used in the Purpose package

;;; Code:

(defcustom purpose-message-on-p nil
  "If non-nil, `purpose-message' will produce a message.
Toggling this on will cause Purpose to produce some debug messages."
  :group 'purpose
  :type 'boolean
  :package-version "1.1.50")

(defun purpose-message (format-string &rest args)
  "Produce a message if `purpose-message-on-p' is non-nil.
The message is produced with the `message' function.  In any case,
return the formatted string. FORMAT-STRING and ARGS are passed to
`message' or `format' as is."
  (if purpose-message-on-p
      (apply #'message format-string args)
    (apply #'format format-string args)))

(if (fboundp 'alist-get)
    ;; alist-get is defined in Emacs 24.4 and newer
    (progn
      (defalias 'purpose-alist-get #'alist-get)
      
      (defun purpose-alist-set (key value alist)
	"Set VALUE to be the value associated to KEY in ALIST.
This doesn't change the original alist, but returns a modified copy."
	(setf (alist-get key alist) value)
	alist)

      (defun purpose-alist-del (key alist)
	"Delete KEY from ALIST.
This doesn't change the original alist, but returns a modified copy."
	;; we could use any value instead of 0, as long as we used it instead
	;; of 0 in both places
	(setf (alist-get key alist 0 t) 0)
	alist))

  ;; define our (limited) version of alist-get for Emacs 24.3 and older
  (defun purpose-alist-get (key alist &optional default remove)
    "Get KEY's value in ALIST.
If no such key, return DEFAULT.
When setting KEY's value, if the new value is equal to DEFAULT and
REMOVE is non-nil, then delete the KEY instead."
    (let ((entry (assq key alist)))
      (if entry
	  (cdr entry)
	default)))
  
  (defun purpose-alist-set (key value alist)
    "Set VALUE to be the value associated to KEY in ALIST.
This doesn't change the original alist, but returns a modified copy."
    (cons (cons key value)
	  (purpose-alist-del key alist)))

  (defun purpose-alist-del (key alist)
    "Delete KEY from ALIST.
This doesn't change the original alist, but returns a modified copy."
    ;; we could use any value instead of 0, as long as we used it instead
    ;; of 0 in both places
    (cl-remove-if #'(lambda (entry)
		      (eq key (car entry)))
		  alist)))

(defun purpose-flatten (seq)
  "Turn a list of lists (SEQ) to one concatenated list."
  (apply #'append seq))

(defun purpose-alist-combine (&rest alists)
  ;; (purpose-flatten alists)
  (let ((result nil))
    (dolist (alist alists)
      (dolist (element alist)
	(unless (assoc (car element) result)
	  (setq result (purpose-alist-set (car element)
					  (cdr element)
					  result)))))
    result))

(defun purpose-plist-values (plist)
  "Return only the values of PLIST, as a list.
PLIST is a property list.
Example:
 (plist-values '(:foo 1 :bar 2)) -> (1 2)"
  (cl-loop for i from 0
	   for item in plist
	   when (oddp i) collect item))

(provide 'purpose-utils)
;;; purpose-utils.el ends here
