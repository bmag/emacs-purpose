;;; purpose-switch.el --- Purpose-aware display handling -*- lexical-binding: t -*-

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'purpose-core)
(require 'purpose-utils)

(defvar purpose-action-function-ignore-buffer-names
  '("*Completions*"
    "*Ido Completions*")
  "Names of buffers for which the default `display-buffer' and
`switch-to-buffer' behavior should not be overridden.  This is a list of
names.")

(defcustom purpose-display-fallback 'pop-up-window
  "Fallback action to use when `purpose--action-function' couldn't
display a buffer.
This should be either `pop-up-window' for displaying the buffer in a new
window, `pop-up-frame' for displaying the buffer in a new frame, `error'
for signalling an error, or nil for using the regular (purpose-less)
`display-buffer' behavior.  Any other value is treated the same as nil."
  :group 'purpose
  :type 'symbol
  :package-version "1.2")

(defcustom purpose-display-buffer-hook nil
  "Hook to run after displaying a buffer with `purpose--action-function'.
This hook is called with one argument - the window used for display."
  :group 'purpose
  :type 'hook
  :package-version "1.2.50")

(defcustom purpose-select-buffer-hook nil
  "Hook to run after selecting a buffer with `purpose-select-buffer'."
  :group 'purpose
  :type 'hook
  :package-version "1.2")

(defvar purpose--active-p nil
  "When nil, Purpose's advices and `purpose--action-function' are not
used.  This is an internal variable, don't set it yourself.")

(defvar purpose--alist nil
  "Variable used by Purpose's display functions for setting an alist for
`purpose--action-function'.  This is an internal variable, don't change
it yourself.")

(defvar purpose-action-sequences
  '((switch-to-buffer . (purpose-display-reuse-window-buffer
			 purpose-display-reuse-window-purpose
			 purpose-display-maybe-same-window
			 purpose-display-maybe-other-window
			 purpose-display-maybe-other-frame
			 purpose-display-maybe-pop-up-window
			 purpose-display-maybe-pop-up-frame))
    (prefer-same-window . (purpose-display-maybe-same-window
			   purpose-display-reuse-window-buffer
			   purpose-display-reuse-window-purpose
			   purpose-display-maybe-other-window
			   purpose-display-maybe-other-frame
			   purpose-display-maybe-pop-up-window
			   purpose-display-maybe-pop-up-frame))
    (force-same-window . (purpose-display-maybe-same-window))
    (prefer-other-window . (purpose-display-reuse-window-buffer
			    purpose-display-reuse-window-purpose
			    purpose-display-maybe-other-window
			    purpose-display-maybe-pop-up-window
			    purpose-display-maybe-other-frame
			    purpose-display-maybe-pop-up-frame
			    purpose-display-maybe-same-window))
    (prefer-other-frame . (purpose-display-reuse-window-buffer-other-frame
			   purpose-display-reuse-window-purpose-other-frame
			   purpose-display-maybe-other-frame
			   purpose-display-maybe-pop-up-frame
			   purpose-display-maybe-other-window
			   purpose-display-maybe-pop-up-window
			   purpose-display-reuse-window-buffer
			   purpose-display-reuse-window-purpose
			   purpose-display-maybe-same-window))))

(defvar purpose-default-action-sequence 'prefer-other-window)

(defvar purpose-special-action-sequences nil
  "This variable makes Purpose handle some buffers differently.
`purpose-special-action-sequences' should be a list.  Each entry in the
list is a list itself, where:
1. the entry's first item (entry's car) is a condition
2. the entry's rest items (entry's cdr) are display functions
Condition is either a purpose, or a predicate function that takes 3
arguments: PURPOSE, BUFFER, ALIST.
When `purpose--action-function' tries to display a buffer, it will try
first the action sequences in `purpose-special-action-sequences' whose
condition was met.")



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

(defun purpose--change-buffer (buffer window type alist)
  "Make window WINDOW display buffer BUFFER, but don't select it."
  (window--display-buffer buffer window type alist))

(defalias 'purpose-display-reuse-window-buffer #'display-buffer-reuse-window)

(defun purpose--reusable-frames (alist)
  "Return a list of reusable frames.
If ALIST contains a `reusable-frames' entry, its value determines which
frames to search for a reusable window:
  nil -- the selected frame
  A frame -- just that frame
  `visible' -- all visible frames
  0 -- all frames on the current terminal
  t -- all frames.

If ALIST contains no `reusable-frames' entry, search just the selected
frame if `pop-up-frames' is nil; search all frames on the current
terminal if it's non-nil."
  (let-alist alist
    (let ((reusable-frames (cond ((assoc 'reusable-frames alist)
				  .reusable-frames)
				 (pop-up-frames 0)
				 (t nil))))
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
	     nil)))))

(defun purpose-display-reuse-window-purpose (buffer alist &optional purpose)
  "Display BUFFER in a window that is already used for purpose PURPOSE.
Return that window.  Return nil if no usable window is found.
Windows that are dediacted to their buffers are not eligible for reuse.

PURPOSE defaults to BUFFER's purpose.

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
    (let* ((frames (purpose--reusable-frames alist))
	   (windows (purpose-flatten (mapcar #'window-list frames)))
	   (purpose (or purpose (purpose-buffer-purpose buffer)))
	   window)
      (setq windows (cl-delete-if
		     #'(lambda (window)
			 (or (window-dedicated-p window)
			     (not (eql purpose
				       (purpose-window-purpose window)))))
		     windows))
      (when .inhibit-same-window
	(setq windows (delq (selected-window) windows)))
      (setq window (car windows))
      (when window
	(purpose--change-buffer buffer window 'reuse alist))
      window)))

(defun purpose-display-reuse-window-buffer-other-frame (buffer alist)
  "Return a window that is already displaying BUFFER.
Return nil if no usable window is found.
Windows in the selected frame are not eligible for reuse, even if
`reusable-frames' says to search the selected frame.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame."
  (let* ((frames (cl-delete (selected-frame)
			    (purpose--reusable-frames alist)))
	 (windows (purpose-flatten (mapcar #'window-list frames)))
	 window)
    (setq windows (cl-delete-if #'window-dedicated-p windows))
    (setq window (car windows))
    (when window
      (purpose--change-buffer buffer window 'reuse alist))
    window))

(defun purpose-display-reuse-window-purpose-other-frame (buffer alist &optional purpose)
  "Display BUFFER in a window that is already used for purpose PURPOSE.
Return that window.  Return nil if no usable window is found.
Windows that are dediacted to their buffers are not eligible for reuse.
Windows in the selected frame are not eligible for reuse, even if
`reusable-frames' says to search the selected frame.

PURPOSE defaults to BUFFER's purpose.

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
  (let* ((frames (cl-delete (selected-frame)
			    (purpose--reusable-frames alist)))
	 (windows (purpose-flatten (mapcar #'window-list frames)))
	 (purpose (or purpose (purpose-buffer-purpose buffer)))
	 window)
    (setq windows (cl-delete-if
		   #'(lambda (window)
		       (or (window-dedicated-p window)
			   (not (eql purpose
				     (purpose-window-purpose window)))))
		   windows))
    (setq window (car windows))
    (when window
      (purpose--change-buffer buffer window 'reuse alist))
    window))

(defun purpose-display-same-window (buffer alist)
  "Display BUFFER in selected window, no matter what.
This function ignores window dedication and any entry in ALIST."
  (purpose--change-buffer buffer (selected-window) 'reuse alist)
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
      ;; (when .inhibit-same-window
      ;; 	(setq windows (delete (selected-window) windows)))
      (setq windows (delete (selected-window) windows))
      (setq window (car windows))
      (when window
	(purpose--change-buffer buffer window 'reuse alist)
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
	(purpose--change-buffer buffer window 'reuse alist)
	window))))

(defun purpose-display-pop-up-window--internal (buffer alist force-split)
  "Display BUFFER in a new window.
If possible, the window is split in a sensible way.  Otherwise, if
FORCE-SPLIT is non-nil, the window is split vertically.
The window that is split is either the largest window, or the least
recently used window.  If couldn't get the largest or least recently
used window, split the selected window."
  (let* ((old-window (or (get-largest-window nil t)
			 (get-lru-window nil t)
			 (selected-window)))
	 (new-window (or (split-window-sensibly old-window)
			 (and force-split
			      (split-window old-window)))))
    (purpose--change-buffer buffer new-window 'window alist)
    new-window))

(defun purpose-display-pop-up-window (buffer alist)
  "Display BUFFER in a new window.
The value of `pop-up-windows' is ignored.  If possible, the window is
split in a sensible way.  Otherwise, it is simply split vertically.
The window that is split is either the largest window, or the least
recently used window.  If couldn't get the largest or least recently
used window, split the selected window."
  (purpose-display-pop-up-window--internal buffer alist t))

(defun purpose-display-maybe-pop-up-window (buffer alist)
  "Display BUFFER in a new window, if possible.
The display is possible if `pop-up-windows' is non-nil.
The display is done with `display-buffer-pop-up-window'."
  (when pop-up-windows
    (purpose-display-pop-up-window--internal buffer alist nil)))

(defalias 'purpose-display-pop-up-frame #'display-buffer-pop-up-frame)

(defun purpose-display-maybe-pop-up-frame (buffer alist)
  "Display BUFFER in a new frame, if possible.
The display is possible if `pop-up-frames' is non-nil (and not `graphic-only'
on a text-only terminal).
The display is done with `display-buffer-pop-up-frame'."
  ;; if `pop-up-frames' is `graphic-only', check `display-graphic-p', otherwise
  ;; check that `pop-up-frames' is non-nil
  (when (if (eq pop-up-frames 'graphic-only)
	    (display-graphic-p)
	  pop-up-frames)
    (purpose-display-pop-up-frame buffer alist)))



;;; Level2 actions

(defun purpose--use-action-function-p (buffer alist)
  "Determine whether `purpose--action-function' should run or not."
  (and
   purpose--active-p
   (not (cdr (assoc 'inhibit-purpose alist)))
   (not (member (buffer-name buffer)
		purpose-action-function-ignore-buffer-names))))

(defun purpose--special-action-sequence (buffer alist)
  "Return special action sequences to use for display BUFFER.
This function loops over list `purpose-special-action-sequences' and for
each entry in the list:
- check if the entry's condition (entry's car):
  a. is equal to BUFFER's purpose, or
  b. is a function that returns non-nil when called with these 3
     arguments: buffer's purpose, BUFFER, ALIST.
- if so, append the entry's action sequence to the result
The function returns a list of display functions that
`purpose--action-function' should use before trying the regular action
sequence."
  (let ((purpose (purpose-buffer-purpose buffer)))
    (purpose-flatten
     (cl-loop
      for (condition . action-sequence) in purpose-special-action-sequences
      when (or (eql purpose condition)
	       (and (functionp condition)
		    (funcall condition purpose buffer alist)))
      collect action-sequence))))

;; Purpose action function (integration with `display-buffer')
(defun purpose--action-function (buffer alist)
  "Action function to use for overriding default display-buffer
behavior.
This function should be used by setting
`display-buffer-overriding-action' to (purpose--action-function . nil).
If ALIST is nil, it is ignored and `purpose--alist' is used instead."
  (setq alist (purpose-alist-combine alist purpose--alist))
  (purpose-message "Purpose display: Buffer: %S; Alist: %S" buffer alist)
  (when (purpose--use-action-function-p buffer alist)
    (let-alist alist
      (let* ((old-frame (selected-frame))
	     (special-action-sequence (purpose--special-action-sequence buffer
									alist))
	     (normal-action-sequence (purpose-alist-get
				      (or .action-order
					  purpose-default-action-sequence)
				      purpose-action-sequences))
	     (action-sequence (append special-action-sequence
				      normal-action-sequence))
	     (new-window
	      ;; call all display actions in action-sequence until one of them
	      ;; succeeds, and return the window used for display (action's
	      ;; return value)
	      (cl-do ((action-sequence action-sequence (cdr action-sequence))
		      (window nil ;; (funcall (car action-sequence) buffer alist)
			      (progn
				(purpose-message "trying: %S" (car action-sequence))
				(funcall (car action-sequence) buffer alist))))
		  ((or (null action-sequence) window) window))))
	(let ((window (if new-window
			  new-window
			(cond
			 ((eql purpose-display-fallback 'pop-up-frame)
			  (purpose-message
			   "trying fallback: purpose-display-pop-up-frame")
			  (purpose-display-pop-up-frame buffer alist))

			 ((eql purpose-display-fallback 'pop-up-window)
			  (purpose-message
			   "trying fallback: purpose-display-pop-up-window")
			  (purpose-display-pop-up-window buffer alist))

			 ((eql purpose-display-fallback 'error)
			  (error "No window available"))

			 (t
			  (purpose-message
			   "falling back to regular display-buffer")
			  nil)))))
	  (when window
	    (prog1 window
	      (run-hook-with-args 'purpose-display-buffer-hook window))))))))

(defun purpose-select-buffer (buffer-or-name &optional action-order norecord)
  "Display buffer BUFFER-OR-NAME in window and then select that window.
ACTION-ORDER is used as the `action-order' entry in
`purpose--action-function''s alist.
This function runs hook `purpose-select-buffer-hook' when its done."
  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
	 (purpose--alist (purpose-alist-set 'action-order
					    action-order
					    purpose--alist))
	 (old-window (selected-window))
	 (old-frame (selected-frame))
	 (new-window (display-buffer buffer-or-name))
	 (new-frame (window-frame new-window)))
    (when new-window
      ;; If we chose another frame, make sure it gets input focus. - taken from
      ;; `pop-to-buffer''s code
      (unless (eq new-frame old-frame)
	(select-frame-set-input-focus new-frame norecord))
      (select-window new-window norecord))
    (run-hooks 'purpose-select-buffer-hook)
    buffer))


;;; Level3 actions

(defun purpose-switch-buffer (buffer-or-name &optional norecord force-same-window)
  "Select buffer BUFFER-OR-NAME, preferably in the selected window.
If FORCE-SAME-WINDOW is non-nil, don't select a different window if the
currently selected window is not available."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  ;; `display-buffer' should call `purpose--action-function', and
  ;; `purpose--action-function' should try to switch buffer in current window,
  ;; and if that's impossible - display buffer in another window.
  (purpose-select-buffer buffer-or-name
			 (if force-same-window
			     'force-same-window
			   'switch-to-buffer)
			 norecord))

(defun purpose-switch-buffer-other-window (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME in another window.
Never selects the currently selected window."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((pop-up-windows t)
	(purpose--alist (purpose-alist-set 'inhibit-same-window
					   t
					   purpose--alist)))
    (purpose-select-buffer buffer-or-name
			   'prefer-other-window
			   norecord)))

(defun purpose-switch-buffer-other-frame (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in another frame."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((pop-up-frames t)
	(purpose--alist (purpose-alist-set 'inhibit-same-window
					   t
					   purpose--alist)))
    (purpose-select-buffer buffer-or-name 'prefer-other-frame norecord)))

(defun purpose-pop-buffer (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in another window."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (purpose-select-buffer buffer-or-name 'prefer-other-window norecord))

(defun purpose-pop-buffer-same-window (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in the selected window."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (purpose-select-buffer buffer-or-name 'prefer-same-window norecord))



;;; Overrides (advices)

;; TODO: maybe recognize some more actions
(defun purpose-display--action-to-order (action)
  "Return appropriate `action-order' value for ACTION."
  (when (not (listp action))		; non-nil, non-list
    'prefer-other-window))

(define-purpose-compatible-advice 'display-buffer
  :around purpose-display-buffer-advice
  (buffer-or-name &optional action frame)
  "Update `purpose--alist' when calling `display-buffer'."
  ;; new style advice
  ((let* ((action-order (purpose-display--action-to-order action))
	  (purpose--alist (if action-order
			      (purpose-alist-set 'action-order
						 action-order
						 purpose--alist)
			    purpose--alist)))
     (funcall oldfun buffer-or-name action frame)))

  ;; old style advice
  ((let* ((action-order (purpose-display--action-to-order action))
	  (purpose--alist (if action-order
			      (purpose-alist-set 'action-order
						 action-order
						 purpose--alist)
			    purpose--alist)))
     ad-do-it)))

(define-purpose-compatible-advice 'switch-to-buffer
  :around purpose-switch-to-buffer-advice
  (buffer-or-name &optional norecord force-same-window)
  "Advice for overriding `switch-to-buffer' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer', otherwise call `switch-to-buffer'."
  ;; new style advice
  ((purpose-message "switch-to-buffer advice")
   ;; check the full `purpose--use-action-function-p' here, because
   ;; if purpose shouldn't be used for some reason (such as
   ;; `purpose-action-function-ignore-buffer-names'), then we want
   ;; to fallback to `switch-to-buffer', instead of
   ;; `display-buffer'
   (if (purpose--use-action-function-p (window-normalize-buffer-to-switch-to buffer-or-name) nil)
       (purpose-switch-buffer buffer-or-name norecord force-same-window)
     (funcall oldfun buffer-or-name norecord force-same-window)))

  ;; old style advice
  ((purpose-message "switch-to-buffer advice")
   (if (purpose--use-action-function-p (window-normalize-buffer-to-switch-to buffer-or-name) nil)
       (purpose-switch-buffer buffer-or-name norecord force-same-window)
     ad-do-it)))

(define-purpose-compatible-advice 'switch-to-buffer-other-window
  :around purpose-switch-to-buffer-other-window-advice
  (buffer-or-name &optional norecord)
  "Advice for overriding `switch-to-buffer-other-window' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer-other-window', otherwise call
`switch-to-buffer-other-window'."
  ;; new style advice
  ((purpose-message "switch-to-buffer-other-window advice")
   (if purpose--active-p
       (purpose-switch-buffer-other-window buffer-or-name norecord)
     (funcall oldfun buffer-or-name norecord)))
  
  ;; old style advice
  ((purpose-message "switch-to-buffer-other-window advice")
   (if purpose--active-p
       (purpose-switch-buffer-other-window buffer-or-name norecord)
     ad-do-it)))

(define-purpose-compatible-advice 'switch-to-buffer-other-frame
  :around purpose-switch-to-buffer-other-frame-advice
  (buffer-or-name &optional norecord)
  "Advice for overriding `switch-to-buffer-other-frame' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer-other-frame', otherwise call
`switch-to-buffer-other-frame'."
  ;; new style advice
  ((purpose-message "switch-to-buffer-other-frame advice")
   (if purpose--active-p
       (purpose-switch-buffer-other-frame buffer-or-name norecord)
     (funcall oldfun buffer-or-name norecord)))

  ;; old style advice
  ((purpose-message "switch-to-buffer-other-frame advice")
   (if purpose--active-p
       (purpose-switch-buffer-other-frame buffer-or-name norecord)
     ad-do-it)))

(define-purpose-compatible-advice 'pop-to-buffer
  :around purpose-pop-to-buffer-advice
  (buffer-or-name &optional action norecord)
  "Advice for overriding `pop-to-buffer' conditionally.
If Purpose is active (`purpose--active-p' is non-nil) and ACTION is nil,
call `purpose-pop-buffer', otherwise call `pop-to-buffer'."
  ;; new style advice
  ((purpose-message "pop-to-buffer advice")
   (if (and purpose--active-p
	    (not action))
       (purpose-pop-buffer buffer-or-name norecord)
     (funcall oldfun buffer-or-name action norecord)))

  ;; old style advice
  ((purpose-message "pop-to-buffer advice")
   (if (and purpose--active-p
	    (not action))
       (purpose-pop-buffer buffer-or-name norecord)
     ad-do-it)))

(define-purpose-compatible-advice 'pop-to-buffer-same-window
  :around purpose-pop-to-buffer-same-window-advice
  (buffer-or-name &optional norecord)
  "Advice for overriding `pop-to-buffer-same-window' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-pop-buffer-same-window', otherwise call
`pop-to-buffer-same-window'."
  ;; new style advice
  ((purpose-message "pop-to-buffer-same-window advice")
   (if purpose--active-p
       (purpose-pop-buffer-same-window buffer-or-name norecord)
     (funcall oldfun buffer-or-name norecord)))
  
  ;; old style advice
  ((purpose-message "pop-to-buffer-same-window advice")
   (if purpose--active-p
       (purpose-pop-buffer-same-window buffer-or-name norecord)
     ad-do-it)))

;; anti-override:

(defmacro without-purpose (&rest body)
  "Make Purpose inactive while executing BODY.
This works internally by temporarily setting `purpose--active-p'."
  `(let ((purpose--active-p nil))
     ,@body))

(defmacro without-purpose-command (command)
  "Create a command that runs COMMAND with purpose inactive.
This works internally by using `without-purpose' and
`call-interactively'."
  `(lambda ()
     (interactive)
     (without-purpose
      (call-interactively ,command))))



;;; Advanced switching functions

(defun purpose-read-buffers-with-purpose (purpose)
  "Prompt the user for a buffer with purpose PURPOSE."
  (ido-completing-read "[PU] Buffer: "
		       (mapcar #'buffer-name
			       (delq (current-buffer)
				     (purpose-buffers-with-purpose purpose)))))

(defun purpose-switch-buffer-with-purpose (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))

(defun purpose-switch-buffer-with-purpose-other-window (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another window.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer-other-window
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))

(defun purpose-switch-buffer-with-purpose-other-frame (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another frame.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer-other-frame
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))

(provide 'purpose-switch)
;;; purpose-switch.el ends here
