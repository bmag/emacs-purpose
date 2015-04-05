;;; window-purpose-utils.el --- Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2015 Bar Magal

;; Author: Bar Magal (2015)
;; Package: purpose

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Utilities used in the Purpose package

;;; Code:

(require 'subr-x)

(defcustom purpose-message-on-p nil
  "If non-nil, `purpose-message' will produce a message.
Toggling this on will cause Purpose to produce some debug messages."
  :group 'purpose
  :type 'boolean
  :package-version "1.2")

(defun purpose-message (format-string &rest args)
  "Produce a message if `purpose-message-on-p' is non-nil.
The message is produced with the `message' function.  In any case,
return the formatted string. FORMAT-STRING and ARGS are passed to
`message' or `format' as is."
  (if purpose-message-on-p
      (apply #'message format-string args)
    (apply #'format format-string args)))

;; define our (limited) version of alist-get
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
		alist))

(defun purpose-flatten (seq)
  "Turn a list of lists (SEQ) to one concatenated list."
  (apply #'append seq))

(defun purpose-alist-combine (&rest alists)
  "Combine ALISTS into one alist.
If several alists have the same key, the entry from the first alist with
that key is used.  Example:
  (purpose-alist-combine '((a . 1) (b . 2)) '((a . 3) (c . 4)))
  => '((a . 1) (b . 2) (c . 4))"
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
	   when (cl-oddp i) collect item))



;;; compatibility layer for advices

(defun purpose-advice-convert-where-arg (where)
  "Convert WHERE argument from new advice style to old advice style.
New style is :before, :after, etc.  Old style is 'before, 'after, etc."
  (unless (keywordp where)
    (signal 'wrong-type-argument `(keywordp ,where)))
  (intern (mapconcat #'identity (cdr (split-string (symbol-name where) ":")) ":")))

(defun purpose-advice-new-style-arglist (arglist where)
  "Convert ARGLIST to new style, according to WHERE.
If WHERE is :around, add 'oldfun to the beginning of ARGLIST.
Otherwise, return ARGLIST without changes."
  (if (eql where :around)
      (append '(oldfun) arglist)
    arglist))

(defmacro define-purpose-compatible-advice (symbol where name arglist docstring new-body old-body)
  "Define advice, using new or old advice style as appropriate.
SYMBOL and WHERE have the same meaning as in `advice-add'.  NAME
has the same meaning as FUNCTION argument of `advice-add'.
ARGLIST has the same meaning as in `defadvice'.  DOCSTRING is the
advice's documentation.  NEW-BODY is the advice's body if the new
advice style is available.  OLD-BODY is the advice's body if the
new advice style is unavailable.

`define-purpose-compatible-advice' properly supports only :around, :before and :after advices."
  (if (fboundp 'advice-add)
      `(defun ,name (,@(purpose-advice-new-style-arglist arglist where))
	 ,docstring
	 ,@new-body)
    ;; ,(cadr symbol) turns <'foo> into <foo>
    `(defadvice ,(cadr symbol) (,(purpose-advice-convert-where-arg where) ,name ,arglist)
       ,docstring
       ,@old-body)))

(defmacro purpose-advice-add (symbol where name)
  "Enable advice, using new or old advice style as appropriate.
SYMBOL, WHERE and NAME have the same meaning as in
`define-purpose-advice'."
  (if (fboundp 'advice-add)
      `(advice-add ,symbol ,where ,name)
    `(progn
       (ad-enable-advice ,symbol ',(purpose-advice-convert-where-arg where) ,name)
       (ad-update ,symbol)
       (ad-activate ,symbol))))

(defmacro purpose-advice-remove (symbol where name)
  "Disable advice, using new or old advice style as appropriate.
SYMBOL, WHERE and NAME have the same meaning as in
`define-purpose-advice'."
  (if (fboundp 'advice-remove)
      `(advice-remove ,symbol ,name)
    `(progn
       (ad-disable-advice ,symbol ',(purpose-advice-convert-where-arg where) ,name)
       (ad-update ,symbol))))

(defun purpose-get-all-purposes ()
  (delete-dups
    (append
      (hash-table-values purpose--default-name-purposes)
      (hash-table-values purpose--default-mode-purposes)
      (hash-table-values purpose--default-regexp-purposes)
      (mapcar 'cdr purpose-user-mode-purposes)
      (mapcar 'cdr purpose-user-name-purposes)
      (mapcar 'cdr purpose-user-regexp-purposes))))

(provide 'window-purpose-utils)
;;; window-purpose-utils.el ends here
