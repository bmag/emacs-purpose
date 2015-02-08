;;; pu-configuration.el

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 1.0

;;; Commentary:
;; This file contains functions for changing Purpose's configuration.
;; The configuration is based on 3 parameters: mapping major modes to
;; purposes, mapping names (exact match) to purposes, and mapping names
;; (regexp match) to purposes.
;; The configuration is stored in internal variables, which should not
;; be changed directly. Instead, use one of the getters, setters or
;; deleters provided (e.g. `pu:get-name-purpose', `pu:set-mode-purpose',
;; `pu:del-name-regexp-purpose').
;; In order to change more than one key at once, use the higher level
;; `set-configuration' and `add-configuration' functions. You could also
;; use `clear-configuration' and `reset-configuration'.
;; To change the default configuration, change the variables
;; `pu:default-name-purposes', `pu:default-mode-purposes' and
;; `pu:default-name-regexp-purposes'.

;;; Code:

;;; Basic configuration handling (set/get/delete single mapping)
(defvar pu:name-purposes (make-hash-table :test 'equal)
  "A hash table holding the configuration of names to purposes. This
variable is intended for internal use, do not set it directly.")

(defvar pu:mode-purposes (make-hash-table)
  "A hash table holding the configuration of major modes to
  purposes. This variable is intended for internal use, do not set it
  directly.")

(defvar pu:name-regexp-purposes (make-hash-table :test 'equal)
  "A hash table holding the configuration of name regexps to
  purposes. This variable is intended for internal use, do not set it
  directly.")

(defvar pu:default-mode-purposes '((comint-mode . terminal)
				   (prog-mode . edit)
				   (image-mode . image)
				   (vc-dir-mode . vc)
				   (vc-log-entry-mode . vc))
  "Default value for `pu:mode-purposes'.")

(defvar pu:default-name-purposes '(("*shell*" . terminal)
				   (".gitignore" . edit)
				   (".hgignore" . edit))
  "Default value for `pu:name-purposes'.")

(defvar pu:default-name-regexp-purposes nil
  "Default value for `pu:name-regexp-purposes'.")



;;; Low level functions for changing configuration
;; setters
(defun pu:set-name-purpose (name purpose)
  "Set the purpose of buffers with name NAME to be PURPOSE.
NAME should be a string."
  (puthash name purpose pu:name-purposes))

(defun pu:set-mode-purpose (mode purpose)
  "Set the purpose of buffers with major mode MODE to be PURPOSE.
This affects also buffers whose major mode is derived from MODE."
  (puthash mode purpose pu:mode-purposes))

(defun pu:set-name-regexp-purpose (regexp purpose)
  "Set the purpose of buffers with names that match regexp REGEXP to be
purpose."
  (puthash regexp purpose pu:name-regexp-purposes))

;; getters
(defun pu:get-name-purpose (name)
  "Get the purpose of buffer-name NAME. Return nil if no purpose is
found."
  (gethash name pu:name-purposes))

(defun pu:get-mode-purpose (mode)
  "Get the purpose of major-mode MODE. Return nil if no purpose is
found."
  (gethash mode pu:mode-purposes))

(defun pu:get-name-regexp-purpose (regexp)
  "Get the purpose of buffer-name matching regexp REGEXP. Return nil if
no purpose is found."
  (gethash regexp pu:name-regexp-purposes))

;; deleters
(defun pu:del-name-purpose (name)
  "Delete the mapping of buffer-name NAME to a purpose."
  (remhash name pu:name-purposes))

(defun pu:del-mode-purpose (mode)
  "Delete the mapping of major-mode MODE to a purpose."
  (remhash mode pu:mode-purposes))

(defun pu:del-name-regexp-purpose (regexp)
  "Delete the mapping of buffer-name regexp REGEXP to a purpose."
  (remhash regexp pu:name-regexp-purposes))



;;; High level configuration handling (UI/API)
(defun pu:clear-configuration ()
  "Delete stored purpose configuration."
  (interactive)
  (clrhash pu:name-purposes)
  (clrhash pu:mode-purposes)
  (clrhash pu:name-regexp-purposes))

(defun pu:add-configuration (new-name-purposes
			     new-mode-purposes
			     new-name-regexp-purposes)
  "Add the mappings defined in alists NEW-NAME-PURPOSES,
NEW-MODE-PURPOSES and NEW-NAME-REGEXP-PURPOSES to the purpose
configuration."
  (mapc #'(lambda (element) (pu:set-name-purpose (car element) (cdr element)))
	new-name-purposes)
  (mapc #'(lambda (element) (pu:set-mode-purpose (car element) (cdr element)))
	new-mode-purposes)
  (mapc #'(lambda (element)
	    (pu:set-name-regexp-purpose (car element) (cdr element)))
	new-name-regexp-purposes))

(defun pu:reset-configuration ()
  "Reset purpose configuration to default configuration."
  (interactive)
  (pu:clear-configuration)
  (pu:add-configuration pu:default-name-purposes
			pu:default-mode-purposes
			pu:default-name-regexp-purposes))

(defun pu:set-configuration (new-name-purposes
			     new-mode-purposes
			     new-name-regexp-purposes
			     &optional dont-use-default)
  "Set the entire purpose configuration according to alists
NEW-NAME-PURPOSES, NEW-MODE-PURPOSES and NEW-NAME-REGEXP-PURPOSES. The
configuration is built on top of the default configuration.
If DONT-USE-DEFAULT is non-nil, the default configuration is not used as
the basis for the configuration."
  (if dont-use-default
      (pu:clear-configuration)
    (pu:reset-configuration))
  (pu:add-configuration new-name-purposes
			new-mode-purposes
			new-name-regexp-purposes))


(pu:reset-configuration)
(provide 'pu-configuration)
