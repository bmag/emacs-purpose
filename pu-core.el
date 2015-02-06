;;; pu-core.el

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 0.1.0

;;; Commentary:
;; This file contains core functions to be used by other parts of
;; package Purpose.

;;; Code:

(require 'pu-configuration)

(defvar default-purpose 'general
  "The default purpose for buffers which didn't get another purpose.")



;;; utilities
(defun pu:iter-hash (function table)
  "Like `maphash', but return a list the results of calling FUNCTION for
each entry."
  (let (results)
    (maphash #'(lambda (key value)
		 (setq results (append results
				       (list (funcall function key value)))))
	     table)
    results))

(defun pu:buffer-major-mode (buffer-or-name)
  "Return the major mode of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun pu:dummy-buffer-name (purpose)
  "Create the name for a dummy buffer with purpose PURPOSE. The name
created is \"*pu-dummy-PURPOSE-*\". e.g. for purpose 'edit, the name is
\"*pu-dummy-edit-*\"."
  (concat "*pu-dummy-" (symbol-name purpose) "*"))

(defun pu:dummy-buffer-purpose (buffer-or-name)
  "Get buffer's purpose for dummy buffers. A dummy buffer is a buffer
with a name that starts with \"*pu-dummy-\" and ends with \"*\". For
example, the buffer \"*pu-dummy-edit*\" is a dummy buffer with the
purpose 'edit."
  (let ((name (if (stringp buffer-or-name)
		  buffer-or-name
		(buffer-name buffer-or-name))))
    (when (and (string-prefix-p "*pu-dummy-" name)
	       (string= "*" (substring name -1)))
      ;; 10 = (length "*pu-dummy-")
      (intern (substring name 10 -1)))))


;;; simple purpose-finding operations for `pu:buffer-purpose'
(defun pu:buffer-purpose-mode (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
mode."
  (when (get-buffer buffer-or-name) ; check if buffer exists
    (let* ((major-mode (pu:buffer-major-mode buffer-or-name))
	   (derived-modes (pu:iter-hash #'(lambda (mode purpose) mode)
					pu:mode-purposes))
	   (derived-mode (apply #'derived-mode-p derived-modes)))
      (when derived-mode
	(pu:get-mode-purpose derived-mode)))))

(defun pu:buffer-purpose-name (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
exact name."
  (pu:get-name-purpose (if (stringp buffer-or-name)
			buffer-or-name
		      (buffer-name buffer-or-name))))

;;TODO: implement
(defun pu:buffer-purpose-name-regexp (buffer-or-name)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by the
regexps matched by its name."
  nil)

(defun pu:buffer-purpose (buffer-or-name)
  "Get buffer's purpose. The purpose is determined by consulting these
functions in this order:
1. `pu:dummy-buffer-purpose'
2. `pu:buffer-purpose-name'
3. `pu:buffer-purpose-name-regexp'
4. `pu:buffer-purpose-mode'
If no purpose was determined, return `default-purpose'."
  (or (pu:dummy-buffer-purpose buffer-or-name)
      (pu:buffer-purpose-name buffer-or-name)
      (pu:buffer-purpose-name-regexp buffer-or-name)
      (pu:buffer-purpose-mode buffer-or-name)
      default-purpose))

(defun pu:buffers-with-purpose (purpose)
  "Return a list of all existing buffers with purpose PURPOSE."
  (remove-if-not #'(lambda (buffer) (eql purpose (pu:buffer-purpose buffer)))
		 (buffer-list)))

(defun pu:window-purpose (&optional window)
  "Get window's purpose. The window's purpose is determined by its
buffer's purpose.
WINDOW defaults to the selected window."
  (pu:buffer-purpose (window-buffer window)))

(defun pu:windows-with-purpose (purpose)
  "Return a list of all live windows with purpose PURPOSE."
  (remove-if-not #'(lambda (window) (eql purpose (pu:window-purpose window)))
		 (window-list)))



;;; purpose-aware buffer low-level functions
(defun pu:get-buffer-create (purpose)
  "Get the first buffer with purpose PURPOSE. If there is no such
buffer, create a dummy buffer with purpose PURPOSE."
  (or (car (pu:buffers-with-purpose purpose))
      (get-buffer-create (pu:dummy-buffer-name purpose))))

(defun pu:set-window-buffer (purpose &optional window)
  "Make WINDOW display first buffer with purpose PURPOSE.
WINDOW must be a live window and defaults to the selected one.
If there is no buffer with purpose PURPOSE, create a dummy buffer with
purpose PURPOSE."
  (set-window-buffer window (pu:get-buffer-create purpose)))



;;; window purpose dedication
(defun pu:set-window-purpose-dedicated-p (window flag)
  "Set window parameter 'purpose-dedicated of window WINDOW to value
FLAG. WINDOW defaults to the selected window."
  (set-window-parameter window 'purpose-dedicated flag))

(defun pu:window-purpose-dedicated-p (&optional window)
  "Return non-nil if window WINDOW is dedicated to its purpose. The
result is determined by window parameter 'purpose-dedicated. WINDOW
defaults to the selected window."
  (window-parameter window 'purpose-dedicated))

(defun pu:toggle-window-purpose-dedicated (&optional window)
  "Toggle window WINDOW's dedication to its purpose on or off. WINDOW
defaults to the selected window."
  (interactive)
  (let ((flag (not (pu:window-purpose-dedicated-p window))))
    ;; window's purpose is displayed in modeline, so no need to print message
    ;; for the user
    ;;      (msg (if flag
    ;;      	  "Window purpose is now dedicated"
    ;; 	         "Window purpose is not dedicated anymore")))
    (pu:set-window-purpose-dedicated-p window flag)
    (force-mode-line-update)
    ;; window's purpose is displayed in modeline, so no need to print message
    ;; for the user
    ;;(message msg)
    flag))

;; not really purpose-related, but helpful for the user
(defun pu:toggle-window-buffer-dedicated (&optional window)
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

(provide 'pu-core)
