;;; purpose-switch.el --- Purpose-aware display handling

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'purpose-core)

(defvar purpose--action-function-active-p nil
  "Controls whether to override default `display-buffer' behavior.")

(defvar purpose-action-function-ignore-buffer-names
  '("*Completions*" "*Ido Completions*")
  "Names of buffers for which the default `display-buffer' behavior
should not be overridden.  This is a list of names.")

(defvar purpose-display-buffer-hook nil
  "Hook to run after displaying a buffer with a purpose-aware function.
That means after `purpose-switch-buffer', `purpose-pop-buffer' and
`purpose--action-function'.")

(defvar purpose--use-original-switch-p nil
  "Variable used by advice `purpose-switch-to-buffer-advice'.
If non-nil, don't override `switch-to-buffer'.  This is an internal
variable, don't change it yourself.")

(defvar purpose--alist nil
  "Variable used by Purpose's display functions for setting an alist for
`purpose--action-function'.  This is an internal variable, don't change
it yourself.")



(defun purpose-flatten (seq)
  "Turn a list of lists (SEQ) to one concatenated list."
  (apply #'append seq))

;;; Advice for switch-to-buffer
(defmacro purpose--with-original-switch (&rest body)
  "Evaluate the forms in BODY.
`switch-to-buffer' is not overriden while BODY is evaluated."
  `(let ((purpose--use-original-switch-p t))
     ,@body))

;; (defun purpose-switch-to-buffer-advice (oldfun buffer-or-name &optional norecord force-same-window use-original-p)
(defun purpose-switch-to-buffer-advice (oldfun buffer-or-name &rest args)
  (message "I'm here! %S" args)
  (if purpose--use-original-switch-p
      (progn
	(message "Use original!")
	(apply oldfun buffer-or-name args))
    (message "Use new!")
    (purpose-switch-buffer buffer-or-name)))



;;; Level1 actions
;; reuse-window-buffer: display buffer in a window already displaying that buffer. frames to consider are chosen by `inhibit-same-window', `reusable-frames', `display-buffer-reuse-frames' and `pop-up-frames'.
;; reuse-window-purpose: display buffer in a window already displaying correct purpose (except buffer-dedicated windows). frames to consider are chosen the same as `reuse-window-buffer'.
;; same-window: display buffer in selected window (regardless of current purpose or buffer)
;; maybe-same-window: display buffer in selected window, if possible (not dedicated)
;; maybe-other-window: display buffer in another window in the selected frame, if possible (not dedicated)
;; maybe-other-frame: display buffer in another window in another frame, if possible (not dedicated)
;; pop-up-window: display buffer in a new window in the selected frame
;; -- how should we split the frame? should we consider other frames as well
;; maybe-pop-up-window: display buffer in a new window in the selected frame, if possible (window can be split)
;; pop-up-frame: display buffer in a new frame

(defun purpose--change-buffer (window buffer)
  "Make window WINDOW display buffer BUFFER, but don't select it."
  (with-selected-window window
    (purpose--with-original-switch
     (switch-to-buffer buffer nil t))))

(defalias 'purpose-display-reuse-window-buffer #'display-buffer-reuse-window)

(defun purpose-display-reuse-window-purpose (buffer alist)
  "Display BUFFER in a window that is already used for the same purpose.
Return that window. Return nil if no usable window is found.
Windows that are dediacted to their buffers are eligible for reuse.

If ALIST has a non-nil `inhibit-same-window' entry, the selected window
is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines which
frames to search for a reusable window:
  nil -- the selected frame
  A frame -- just that frame
  `visible' -- all visible frames
  0 -- all frames on the current terminal
  t -- all frames.

If ALIST contains no `reusable-frames' entry, search just the selected
frame if `pop-up-frames' is nil; search all frames on the current
terminal if it's non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the event
that a window on another frame is chosen, avoid raising that frame."
  (let-alist alist
    (let* ((reusable-frames (cond ((assoc 'reusable-frames alist)
				   .reusable-frames)
				  (pop-up-frames 0)
				  (t nil)))
	   (frames
	    (cond ((null reusable-frames)
		   (list (selected-frame)))
		  ((framep reusable-frames)
		   (list reusable-frames))
		  ((eql reusable-frames 'visible)
		   (visible-frame-list))
		  ((eql reusable-frames 0)
		   (cl-remove-if-not
		    #'(lambda (frame)
			(eql (frame-terminal frame) (frame-terminal)))
		    (frame-list)))
		  ((eql reusable-frames t)
		   (frame-list))
		  (t
		   (message "Bad value for reusable-frames in ALIST: %S"
			    reusable-frames)
		   nil)))
	   (purpose (purpose-buffer-purpose buffer))
	   (windows (purpose-flatten (mapcar #'window-list frames)))
	   window)
      (setq windows (cl-delete-if-not
		     #'(lambda (window)
			 (and (not (window-dedicated-p window))
			      (eql purpose (purpose-window-purpose window))))
		     windows))
      (when .inhibit-same-window
	(setq windows (delq (selected-window) windows)))
      (setq window (car windows))
      (when window
	(purpose--change-buffer window buffer))
      window)))

(defun purpose-display-same-window (buffer alist)
  "Display BUFFER in selected window, no matter what.
This function ignores window dedication and any entry in ALIST."
  (purpose--change-buffer (selected-window) buffer)
  (selected-window))

(defun purpose-display-maybe-same-window (buffer alist)
  "Display BUFFER in selected window, if possible.
Return selected window if BUFFER was displayed, otherwise nil.
It is not possible to display BUFFER in selected window if any of
following is true:
- selected window is dedicated to its buffer, and that buffer is not
  BUFFER itself
- selected window is dedicated to its purpose, and BUFFER has a
  different purpose
- entry `inhibit-same-window' in ALIST is non-nil"
  (let-alist alist
    (unless (or (window-dedicated-p)
		(and (purpose-window-purpose-dedicated-p)
		     (not (eql (purpose-window-purpose)
			       (purpose-buffer-purpose buffer))))
		.inhibit-same-window)
      (purpose-display-same-window buffer alist))))

(defun purpose-display--frame-usable-windows (frame buffer)
  "Return windows in FRAME that can be used to display BUFFER.
Possible windows to use match these requirements:
- window is not dediacted to its buffer
- window is not dediacted to its purpose, or BUFFER has the same purpose

FRAME defaults to the selected frame."
  (cl-remove-if-not
   #'(lambda (window)
       (and (or (not (window-dedicated-p window))
		(eql (window-buffer window) buffer))
	    (or (not (purpose-window-purpose-dedicated-p window))
		(eql (purpose-window-purpose window)
		     (purpose-buffer-purpose buffer)))))
   (window-list frame)))

(defun purpose-display-maybe-other-window (buffer alist)
  "Disply BUFFER in another window in the selected frame, if possible.
Return that window. Return nil if no usable window is found.
Possible windows to use match these requirements:
- window is not dedicated to its buffer
- window is not dedicated to its purpose, or BUFFER has the same purpose"
  (let-alist alist
    (let ((windows (purpose-display--frame-usable-windows nil buffer))
	  window)
      (when .inhibit-same-window
	(setq windows (delete (selected-window) windows)))
      (setq window (car windows))
      (when window
	(purpose--change-buffer window buffer)
	window))))

(defun purpose-display-maybe-other-frame (buffer alist)
  "Display BUFFER in another window in another frame, if possible.
Return that window. Return nil if no usable window is found.
Possible windows to use match these requirements:
- window is not dedicated to its buffer
- window is no dedicated to its purpose, or BUFFER has the same purpose

This function doesn't raise the new frame."
  (let-alist alist
    (let* ((windows (purpose-flatten
		    (mapcar
		     #'(lambda (frame)
			 (purpose-display--frame-usable-windows frame buffer))
		     (remove (selected-frame) (frame-list)))))
	   (window (car windows)))
      (when window
	(purpose--change-buffer window buffer)
	window))))

;;; Level2 actions
;; display-buffer:
;; switch-buffer: `reuse-window-buffer', `reuse-window-purpose', `maybe-same-window'
 

(defun purpose--use-action-function-p (buffer alist)
  "Determine whether `purpose--action-function' should run or not."
  (and
   purpose--action-function-active-p
   (not (cdr (assoc 'inhibit-purpose alist)))
   (not (member (buffer-name buffer)
		purpose-action-function-ignore-buffer-names))))

;; Purpose action function (integration with `display-buffer')
(defun purpose--action-function (buffer alist)
  "Action function to use for overriding default display-buffer
behavior.
This function should be used by setting
`display-buffer-overriding-action' to (purpose--action-function . nil).
If ALIST is nil, it is ignored and `purpose--alist' is used instead."
  (setq alist (or alist purpose--alist))
  (when (purpose--use-action-function-p buffer alist)
    (message "Selected Window: %S; Buffer: %S; Alist: %S" (selected-window) buffer alist)
    ;;TODO: smarter decision between switch and pop, and if to use
    ;;      :reuse-current-window
    ;; (let-alist alist
    ;;   (purpose-pop-buffer buffer :reuse-current-window (not .inhibit-same-window)))
    ;;(purpose--create-buffer-window buffer)
    (let-alist alist
      (let ((old-frame (selected-frame))
	    (new-window
	     (or
	      (purpose-display-reuse-window-buffer buffer alist)
	      (purpose-display-reuse-window-purpose buffer alist)
	      (purpose-display-maybe-same-window buffer alist)
	      (purpose-display-maybe-other-window buffer alist)
	      (purpose-display-maybe-other-frame buffer alist))))
	(if new-window
	    (progn
	      (unless (or (eql (window-frame new-window) old-frame)
			  .inhibit-switch-frame)
		(select-frame-set-input-focus (window-frame new-window)))
	      (select-window new-window))
	  (error "No window available"))))))


;;; UI/API functions
(defun purpose-switch-buffer (buffer-or-name &optional norecord force-same-window)
  "Try to switch to buffer BUFFER-OR-NAME in current window.
If it fails, pop to the buffer in another window."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  ;; `display-buffer' should call `purpose--action-function', and
  ;; `purpose--action-function' should try to switch buffer in current window,
  ;; and if that's impossible - display buffer in another window.
  (display-buffer buffer-or-name))

(defun purpose-pop-buffer (buffer-or-name &optional action norecord)
  "Try to switch to buffer BUFFER-OR-NAME in another window."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((purpose--alist '((inhibit-same-window . t))))
    (display-buffer buffer-or-name)))

(defmacro purpose-with-action-function-inactive (&rest body)
  "Make `purpose--action-function' inactive while evaluating BODY.
This is done by setting `purpose--action-function-active-p' to nil
temporarily."
  `(let ((purpose--action-function-active-p nil))
     ,@body))

(provide 'purpose-switch)
;;; purpose-switch.el ends here
