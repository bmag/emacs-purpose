;;; purpose-configuration.el --- Configuration handling for Purpose

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:
;; This file contains functions for changing Purpose's configuration.
;; The configuration is based on 3 parameters: mapping major modes to
;; purposes, mapping names (exact match) to purposes, and mapping names
;; (regexp match) to purposes.
;; The configuration is stored in internal variables, which should not
;; be changed directly. Instead, use one of the getters, setters or
;; deleters provided (e.g. `purpose-get-name-purpose', `purpose-set-mode-purpose',
;; `purpose-del-name-regexp-purpose').
;; In order to change more than one key at once, use the higher level
;; `set-configuration' and `add-configuration' functions. You could also
;; use `clear-configuration' and `reset-configuration'.
;; To change the default configuration, change the variables
;; `purpose-default-name-purposes', `purpose-default-mode-purposes' and
;; `purpose-default-name-regexp-purposes'.

;;; Code:

;;; Basic configuration handling (set/get/delete single mapping)
(defvar purpose-name-purposes (make-hash-table :test 'equal)
  "A hash table holding the configuration of names to purposes.
This variable is intended for internal use, do not set it directly.")

(defvar purpose-mode-purposes (make-hash-table)
  "A hash table holding the configuration of major modes to purposes.
This variable is intended for internal use, do not set it directly.")

(defvar purpose-name-regexp-purposes (make-hash-table :test 'equal)
  "A hash table holding the configuration of name regexps to purposes.
This variable is intended for internal use, do not set it directly.")

(defvar purpose-default-mode-purposes
  '((comint-mode . terminal)
    (prog-mode . edit)
    (image-mode . image)
    (vc-dir-mode . vc)
    (vc-log-entry-mode . vc))
  "Default value for `purpose-mode-purposes'.")

(defvar purpose-default-name-purposes
  '(("*shell*" . terminal)
    (".gitignore" . edit)
    (".hgignore" . edit))
  "Default value for `purpose-name-purposes'.")

(defvar purpose-default-name-regexp-purposes nil
  "Default value for `purpose-name-regexp-purposes'.")



;;; Low level functions for changing configuration
;; setters
(defun purpose-set-name-purpose (name purpose)
  "Set the purpose of buffers with name NAME to be PURPOSE.
NAME should be a string."
  (puthash name purpose purpose-name-purposes))

(defun purpose-set-mode-purpose (mode purpose)
  "Set the purpose of buffers with major mode MODE to be PURPOSE.
This affects also buffers whose major mode is derived from MODE."
  (puthash mode purpose purpose-mode-purposes))

(defun purpose-set-name-regexp-purpose (regexp purpose)
  "Set the purpose of buffers with names that match regexp REGEXP to be
PURPOSE."
  (puthash regexp purpose purpose-name-regexp-purposes))

;; getters
(defun purpose-get-name-purpose (name)
  "Get the purpose of a buffer named NAME.
Return nil if no purpose is found."
  (gethash name purpose-name-purposes))

(defun purpose-get-mode-purpose (mode)
  "Get the purpose of major-mode MODE.
Return nil if no purpose is found."
  (gethash mode purpose-mode-purposes))

(defun purpose-get-name-regexp-purpose (regexp)
  "Get the purpose of a buffer-name that matches regexp REGEXP.
Return nil if no purpose is found."
  (gethash regexp purpose-name-regexp-purposes))

;; deleters
(defun purpose-del-name-purpose (name)
  "Delete the mapping of buffer-name NAME to a purpose."
  (remhash name purpose-name-purposes))

(defun purpose-del-mode-purpose (mode)
  "Delete the mapping of major-mode MODE to a purpose."
  (remhash mode purpose-mode-purposes))

(defun purpose-del-name-regexp-purpose (regexp)
  "Delete the mapping of buffer-name regexp REGEXP to a purpose."
  (remhash regexp purpose-name-regexp-purposes))



;;; High level configuration handling (UI/API)
(defun purpose-clear-configuration ()
  "Delete stored purpose configuration."
  (interactive)
  (clrhash purpose-name-purposes)
  (clrhash purpose-mode-purposes)
  (clrhash purpose-name-regexp-purposes))

(defun purpose-add-configuration (new-name-purposes
				  new-mode-purposes
				  new-name-regexp-purposes)
  "Add the mappings defined in alists NEW-NAME-PURPOSES,
NEW-MODE-PURPOSES and NEW-NAME-REGEXP-PURPOSES to the purpose
configuration."
  (mapc #'(lambda (element)
	    (purpose-set-name-purpose (car element) (cdr element)))
	new-name-purposes)
  (mapc #'(lambda (element)
	    (purpose-set-mode-purpose (car element) (cdr element)))
	new-mode-purposes)
  (mapc #'(lambda (element)
	    (purpose-set-name-regexp-purpose (car element) (cdr element)))
	new-name-regexp-purposes))

(defun purpose-reset-configuration ()
  "Reset purpose configuration to default configuration."
  (interactive)
  (purpose-clear-configuration)
  (purpose-add-configuration purpose-default-name-purposes
			     purpose-default-mode-purposes
			     purpose-default-name-regexp-purposes))

(defun purpose-set-configuration (new-name-purposes
				  new-mode-purposes
				  new-name-regexp-purposes
				  &optional dont-use-default)
  "Set the entire purpose configuration according to alists
NEW-NAME-PURPOSES, NEW-MODE-PURPOSES and NEW-NAME-REGEXP-PURPOSES.
The configuration is built on top of the default configuration.
If DONT-USE-DEFAULT is non-nil, the default configuration is not used as
the basis for the configuration."
  (if dont-use-default
      (purpose-clear-configuration)
    (purpose-reset-configuration))
  (purpose-add-configuration new-name-purposes
			     new-mode-purposes
			     new-name-regexp-purposes))


(purpose-reset-configuration)
(provide 'purpose-configuration)
;;; purpose-configuration.el ends here
