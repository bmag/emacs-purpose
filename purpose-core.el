;;; purpose-core.el --- Core functions for Purpose

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:
;; This file contains core functions to be used by other parts of
;; package Purpose.

;;; Code:

(require 'purpose-configuration)

(defvar default-purpose 'general
  "The default purpose for buffers which didn't get another purpose.")


;;; utilities
(defun purpose--iter-hash (function table)
  "Like `maphash', but return a list the results of calling FUNCTION
for each entry in hash-table TABLE."
  (let (results)
    (maphash #'(lambda (key value)
		 (setq results
		       (append results
			       (list (funcall function key value)))))
	     table)
    results))

(defun purpose--buffer-major-mode (buffer-or-name)
  "Return the major mode of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun purpose--dummy-buffer-name (purpose)
  "Create the name for a dummy buffer with purpose PURPOSE.
The name created is \"*pu-dummy-PURPOSE-*\".  e.g. for purpose 'edit,
the name is \"*pu-dummy-edit-*\"."
  (concat "*pu-dummy-" (symbol-name purpose) "*"))

(defun purpose--dummy-buffer-purpose (buffer-or-name)
  "Get buffer's purpose for dummy buffers.
A dummy buffer is a buffer with a name that starts with \"*pu-dummy-\"
and ends with \"*\".  For example, the buffer \"*pu-dummy-edit*\" is a
dummy buffer with the purpose 'edit."
  (let ((name (if (stringp buffer-or-name)
		  buffer-or-name
		(buffer-name buffer-or-name))))
    (when (and (string-prefix-p "*pu-dummy-" name)
	       (string= "*" (substring name -1)))
      ;; 10 = (length "*pu-dummy-")
      (intern (substring name 10 -1)))))


;;; simple purpose-finding operations for `purpose-buffer-purpose'
(defun purpose--buffer-purpose-mode (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
mode."
  (when (get-buffer buffer-or-name)	; check if buffer exists
    (let* ((major-mode (purpose--buffer-major-mode buffer-or-name))
	   (derived-modes (purpose--iter-hash #'(lambda (mode purpose) mode)
					      purpose-mode-purposes))
	   (derived-mode (apply #'derived-mode-p derived-modes)))
      (when derived-mode
	(purpose-get-mode-purpose derived-mode)))))

(defun purpose--buffer-purpose-name (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
exact name."
  (purpose-get-name-purpose (if (stringp buffer-or-name)
				buffer-or-name
			      (buffer-name buffer-or-name))))

(defun purpose--buffer-purpose-name-regexp-1 (buffer-or-name regexp purpose)
  "Return purpose PURPOSE if buffer BUFFER-OR-NAME's name matches
regexp REGEXP."
  (when (string-match-p regexp (or (and (bufferp buffer-or-name)
					(buffer-name buffer-or-name))
				   buffer-or-name))
    purpose))

(defun purpose--buffer-purpose-name-regexp (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by the
regexps matched by its name."
  (car (remove nil
	       (purpose--iter-hash
		#'(lambda (regexp purpose)
		    (purpose--buffer-purpose-name-regexp-1 buffer-or-name
							   regexp
							   purpose))
		purpose-name-regexp-purposes))))

(defun purpose-buffer-purpose (buffer-or-name)
  "Get the purpose of buffer BUFFER-OR-NAME.
The purpose is determined by consulting these functions in this order:
1. `purpose--dummy-buffer-purpose'
2. `purpose--buffer-purpose-name'
3. `purpose--buffer-purpose-name-regexp'
4. `purpose--buffer-purpose-mode'
If no purpose was determined, return `default-purpose'."
  (or (purpose--dummy-buffer-purpose buffer-or-name)
      (purpose--buffer-purpose-name buffer-or-name)
      (purpose--buffer-purpose-name-regexp buffer-or-name)
      (purpose--buffer-purpose-mode buffer-or-name)
      default-purpose))

(defun purpose-buffers-with-purpose (purpose)
  "Return a list of all existing buffers with purpose PURPOSE."
  (cl-remove-if-not #'(lambda (buffer)
			(eql purpose (purpose-buffer-purpose buffer)))
		    (buffer-list)))

(defun purpose-window-purpose (&optional window)
  "Get the purpose of window WINDOW.
The window's purpose is determined by its buffer's purpose.
WINDOW defaults to the selected window."
  (purpose-buffer-purpose (window-buffer window)))

(defun purpose-windows-with-purpose (purpose)
  "Return a list of all live windows with purpose PURPOSE."
  (cl-remove-if-not #'(lambda (window)
			(eql purpose (purpose-window-purpose window)))
		    (window-list)))



;;; purpose-aware buffer low-level functions
(defun purpose--get-buffer-create (purpose)
  "Get the first buffer with purpose PURPOSE.
If there is no such buffer, create a dummy buffer with purpose
PURPOSE."
  (or (car (purpose-buffers-with-purpose purpose))
      (get-buffer-create (purpose--dummy-buffer-name purpose))))

(defun purpose--set-window-buffer (purpose &optional window)
  "Make WINDOW display first buffer with purpose PURPOSE.
WINDOW must be a live window and defaults to the selected one.
If there is no buffer with purpose PURPOSE, create a dummy buffer with
purpose PURPOSE."
  (set-window-buffer window (purpose--get-buffer-create purpose)))



;;; window purpose dedication
(defun purpose-set-window-purpose-dedicated-p (window flag)
  "Set window parameter 'purpose-dedicated of window WINDOW to value
FLAG.
WINDOW defaults to the selected window."
  (set-window-parameter window 'purpose-dedicated flag))

(defun purpose-window-purpose-dedicated-p (&optional window)
  "Return non-nil if window WINDOW is dedicated to its purpose.
The result is determined by window parameter 'purpose-dedicated.
WINDOW defaults to the selected window."
  (window-parameter window 'purpose-dedicated))

(defun purpose-toggle-window-purpose-dedicated (&optional window)
  "Toggle window WINDOW's dedication to its purpose on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let ((flag (not (purpose-window-purpose-dedicated-p window))))
    ;; window's purpose is displayed in modeline, so no need to print message
    ;; for the user
    ;;      (msg (if flag
    ;;      	  "Window purpose is now dedicated"
    ;; 	         "Window purpose is not dedicated anymore")))
    (purpose-set-window-purpose-dedicated-p window flag)
    (force-mode-line-update)
    ;; window's purpose is displayed in modeline, so no need to print message
    ;; for the user
    ;;(message msg)
    flag))

;; not really purpose-related, but helpful for the user
(defun purpose-toggle-window-buffer-dedicated (&optional window)
  "Toggle window WINDOW's dedication to its current buffer on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let* ((flag (not (window-dedicated-p window)))
	 (msg (if flag
		  "Window buffer is now dedicated"
		"Window buffer is not dedicated anymore")))
    (set-window-dedicated-p window flag)
    (message msg)
    flag))

(provide 'purpose-core)
;;; purpose-core.el ends here
