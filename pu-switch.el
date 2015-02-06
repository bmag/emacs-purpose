;;; pu-switch.el

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 0.1.0

;;; Commentary:
;; This file contains functions for switching buffers in a way that
;; respects the purposes of windows and buffers. The main functions are
;; as follows:
;; -  `pu:switch-buffer': Shows a buffer in the current window or in a
;;    matching existing window. For the exact rules, see the function's
;;    documentation.
;; -  `pu:pop-buffer': Shows a buffer in a matching window, but not in
;;    the current window. If necessary, create a new window. For the
;;    exact rules, see the function's documentation.
;; -  `pu:find-file': Opens a file in a window. The window is chosen by
;;    `pu:switch-buffer'.
;; -  `pu:find-file-other-window': Opens a file in a window. The window
;;    is chosen by `pu:pop-buffer'.
;; -  `pu:action-function': Overrides the default behavior of
;;    `display-buffer'. Uses `pu:switch-buffer' and `pu:pop-buffer' to
;;    display the buffer in the correct window.
;; -  `pu:use-action-function-p': Sometimes, we don't want to override
;;    `display-buffer''s default behavior. `pu:use-action-function-p'
;;    determines when to override.
;; -  `pu:display-buffer-hook': This hook is run every time a buffer is
;;    displayed with the help of `pu:switch-buffer' or `pu:pop-buffer'.
;; For now, frames are not taken into consideration, so the behavior may
;; be unexpected when using more than one frame.

;;; Code:

(require 'pu-core)

(defvar pu:action-function-active-p nil
  "Controls whether to override default display-buffer behavior.")

(defvar pu:action-function-ignore-buffer-names '("*Completions*"
						 "*Ido Completions*")
  "Names of buffers for which the default display-buffer behavior should
not be overridden. This is a list of names.")

(defvar pu:display-buffer-hook nil
  "Hook to run after displaying a buffer with a purpose-aware function.
That means after `pu:switch-buffer', `pu:pop-buffer' and
`pu:action-function'.")



;;; Internal functions
(defun pu:choose-window-for-switch (buffer-or-name)
  "Choose a window in which to display buffer BUFFER-OR-NAME. The choice
is made according to the rules in `pu:switch-buffer'."
  (let ((buffer-purpose (pu:buffer-purpose buffer-or-name)))
    (or
     ;; buffer is already shown in a window
     (get-buffer-window buffer-or-name)
     ;; current window matches purpose and is not buffer-dedicated
     (and (not (window-dedicated-p))
	  (eql (pu:window-purpose) buffer-purpose)
	  (selected-window))
     ;; other window matches purpose and is not buffer-dedicated
     (car (remove-if #'(lambda (window) (window-dedicated-p window))
		     (pu:windows-with-purpose buffer-purpose)))
     ;; current window not buffer-dedicated or purpose-dedicated
     (and (not (window-dedicated-p))
	  (not (pu:window-purpose-dedicated-p))
	  (selected-window)))))

(defun pu:choose-window-for-pop (buffer-or-name &optional reuse-current-window)
  "Choose a window in which to display buffer BUFFER-OR-NAME. The choice
is made according to the rules in `pu:pop-buffer'."
  (let ((buffer-purpose (pu:buffer-purpose buffer-or-name)))
    (or
     ;; buffer is displayed in some buffer
     (let ((new-window (get-buffer-window buffer-or-name)))
       (unless (and (not reuse-current-window)
		    (eql new-window (selected-window)))
	 new-window))
     ;; other window matches purpose and is not buffer-dedicated
     (car (remove-if #'(lambda (window)
			 (or (eql window (selected-window))
			     (window-dedicated-p window)))
		     (pu:windows-with-purpose buffer-purpose)))
     ;; other window not buffer-dedicated or purpose-dedicated
     (car (remove-if #'(lambda (window)
			 (or (eql window (selected-window))
			     (window-dedicated-p window)
			     (pu:window-purpose-dedicated-p window)))
		     (window-list))))))

(defun pu:create-buffer-window (buffer-or-name)
  "Create a new window for displaying buffer BUFFER-OR-NAME. Return the
new window. This function also sets window parameter 'quit-restore, if
necessary."
  (let ((old-window (selected-window))
	;; create new window
	(new-window (or (split-window-sensibly)
			(split-window))))
    (when new-window
      (select-window new-window)
      (switch-to-buffer buffer-or-name nil t)
      ;; change window parameters for correct quit-window-restore behavior
      (set-window-parameter new-window 'quit-restore
			    (list 'window 'window old-window
				  (get-buffer-create buffer-or-name)))
      (set-window-prev-buffers new-window nil)
      (set-window-next-buffers new-window nil)
      new-window)))

(defun pu:use-action-function-p (buffer alist)
  "Determine whether `pu:action-function' should run or not."
  (and
   pu:action-function-active-p
   (not (cdr (assoc 'inhibit-purpose alist)))
   (not (member (buffer-name buffer) pu:action-function-ignore-buffer-names))))

;; Purpose action function (integration with `display-buffer')
(defun pu:action-function (buffer alist)
  "Action function to use for overriding default display-buffer
behavior. This function should be used by setting
`display-buffer-overriding-action' to (pu:action-function . nil)."
  (when (pu:use-action-function-p buffer alist)
    (message "Alist: %S" alist)
    ;;TODO: smarter decision between switch and pop, and if to use
    ;;      :reuse-current-window
    
    ;;(pu:switch-buffer buffer)
    (pu:pop-buffer buffer :reuse-current-window t)
    (get-buffer-window buffer)))



;;; UI/API functions
(defun pu:switch-buffer (buffer-or-name)
  "Switch to buffer BUFFER-OR-NAME in a window chosen according to its
purpose. The window is chosen as follows:
1. If the buffer is already shown in a window, use that window.
2. If the current window matches the buffer's purpose, and the window is
   not dedicated to its current buffer, use the current window.
3. If there is another window matching the buffer's purpose, and the
   window is not dedicated to its current buffer, use that window.
4. If the current window is not dedicated to its buffer or its purpose,
   use the current window.
5. Choose a window according to `pu:pop-buffer'.

Return the buffer switched to.
Runs the hook `pu:display-buffer-hook' after switching to the buffer."

  (interactive 
   (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((new-window (pu:choose-window-for-switch buffer-or-name)))
    (if new-window
	(prog2
	    (select-window new-window)
	    (switch-to-buffer buffer-or-name nil t)
	  (run-hooks 'pu:display-buffer-hook))
      (pu:pop-buffer buffer-or-name))))

(defun* pu:pop-buffer (buffer-or-name &key reuse-current-window)
  "Pop to buffer BUFFER-OR-NAME in a window chosen according to its
purpose. The window is chosen as follows: (note that the current window
is never chosen)
1. If the buffer is already shown in a window, use that window. If
   argument REUSE-SAME-WINDOW is non-nil, the current window is
   considered.
2. If there is another window matching the buffer's purpose, and the
   window is not dedicated to its current buffer, use that window.
3. If there is another window which is not dedicated to its current
   buffer or its purpose, use that window.
4. Split current window.

Return the buffer switched to.
Runs the hook `pu:display-buffer-hook' after popping to the buffer."
  (interactive
   (list (read-buffer-to-switch "[PU] Pop to buffer: ")))
  (let ((new-window (pu:choose-window-for-pop buffer-or-name
					      reuse-current-window)))
    (if new-window
	(prog2
	    (select-window new-window)
	    (switch-to-buffer buffer-or-name nil t)
	  (run-hooks 'pu:display-buffer-hook))
      ;; couldn't find other window, must create a new one
      (if (pu:create-buffer-window buffer-or-name)
	  (prog1
	      (current-buffer)
	    (run-hooks 'pu:display-buffer-hook))
	(error "Couldn't create new window")))))

(defun pu:find-file (filename &optional wildcards)
  "Open file FILENAME in a window chosen by the same rules as
`pu:switch-buffer'. If argument WILDCARDS is non-nil, allow processing
of wildcards. This means several files could be opened at once."
  (interactive (find-file-read-args "[PU] Find file: "
				    (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar #'pu:switch-buffer (nreverse value))
      (pu:switch-buffer value))))

(defun pu:find-file-other-window (filename &optional wildcards)
  "Open file FILENAME in a window chosen by the same rules as
`pu:pop-buffer'. If argument WILDCARDS is non-nil, allow processing of
wildcards. This means several files could be opened at once."
  (interactive (find-file-read-args "[PU] Find file in other window: "
				    (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (pu:pop-buffer (car value))
	  (mapc #'pu:switch-buffer (cdr value))
	  value)
      (pu:pop-buffer value))))

(defmacro pu:with-action-function-inactive (&rest body)
  "Make `pu:action-function' inactive while evaluating BODY. This is
done by setting `pu:action-function-active-p' to nil temporarily."
  `(let ((pu:action-function-active-p nil))
     ,@body))

(provide 'pu-switch)
