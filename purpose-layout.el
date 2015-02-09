;;; purpose-layout.el --- Save and load window layout

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:
;; This file contains function for saving and loading the entire window
;; layout.

;;; Code:

(require 'cl-lib)
(require 'purpose-core)

(defvar purpose-default-layout-file
  (concat user-emacs-directory ".purpose-layout")
  "Default file for saving/loading purpose layout.")

(defvar purpose-get-extra-window-params-function nil
  "If non-nil, this variable should be a function.
This variable is used by `purpose-window-params'.  See
`purpose-window-params' for more details.")

(defvar purpose-set-window-properties-functions nil
  "Hook to run after calling `purpose-set-window-properties'.
Use this to set additional properties for windows as they are created,
when `purpose-set-layout' or `purpose-load-layout' is called.  Each
function in `purpose-set-window-properties-functions' is called with two
arguments: PROPERTIES and WINDOW.  PROPERTIES is the window's property
list as saved in the used layout, and WINDOW is the new window.  If
WINDOW is nil, your function should act on the selected window
instead.")



;;; window-params low-level functions
(defun purpose--window-edges-to-percentage (&optional window)
  (cl-multiple-value-bind (left top right bottom) (window-edges window)
    (let ((frame-width (frame-width (window-frame window)))
	  (frame-height (frame-height (window-frame window))))
      (list (/ left frame-width 1.0)
	    (/ top frame-height 1.0)
	    (/ right frame-width 1.0)
	    (/ bottom frame-height 1.0)))))

(defun purpose--window-width-to-percentage (&optional window)
n  (/ (window-width window) (frame-width (window-frame window)) 1.0))

(defun purpose--window-height-to-percentage (&optional window)
  (/ (window-height window) (frame-height (window-frame window)) 1.0))

(defun purpose--window-percentage-to-width (percentage &optional window)
  (round (* percentage (frame-width (window-frame window)))))

(defun purpose--window-percentage-to-height (percentage &optional window)
  (round (* percentage (frame-height (window-frame window)))))



;;; window-params high-level functions
(defun purpose-window-params-p (obj)
  "Return non-nil if OBJ is a window-params plist.
A window-params plist is a plist that is given by
`purpose-window-params'."
  (and (listp obj)
       (plist-member obj :purpose)))

(defun purpose-window-params (&optional window)
  "Return a plist containing the window parameters that are relevant for
Purpose plugin.
These parameters are :purpose, :purpose-dedicated, :width, :height and
:edges.
:purpose is the window's purpose.
:purpose-dedicated corresponds to WINDOW's window parameter of the same
name.
:width is the width of the window as a percentage of the frame's width.
:height is the height of the window as a percentage of the frame's
height.
:edges is also given in percentages.

WINDOW defaults to the selected window.

If the variable `purpose-get-extra-window-params-function' is non-nil,
it should be a function that receives a window as an optional argument
and returns a plist.  That plist is concatenated into the plist that
`purpose-window-params' returns.  The plist returned by
`purpose-get-extra-window-params-function' shouldn't contain any of the
keys described above (:purpose, :purpose-dedicated, :width, :height,
:edges).  If it does, the behavior is not defined."
  (let ((buffer (window-buffer window)))
    (append
     (list :purpose (purpose-buffer-purpose buffer)
	   :purpose-dedicated (purpose-window-purpose-dedicated-p window)
	   :width (purpose--window-width-to-percentage window)
	   :height (purpose--window-height-to-percentage window)
	   :edges (purpose--window-edges-to-percentage window))
     (and purpose-get-extra-window-params-function
	  (funcall purpose-get-extra-window-params-function window)))))

(defun purpose-set-window-properties (properties &optional window)
  "Set the buffer and window-parameters of window WINDOW, according to
property list PROPERTIES.
This function runs `purpose-set-window-properties-functions' when it
finishes."
  (purpose--set-window-buffer (plist-get properties :purpose) window)
  (purpose-set-window-purpose-dedicated-p window (plist-get properties
							    :purpose-dedicated))
  (run-hook-with-args 'purpose-set-window-properties-functions
		      properties window))



;;; Low level functions for changing the layout
(defun purpose--split-window (tree window)
  "Split window WINDOW to the amount of child windows it contains.
TREE is a window tree (see `window-tree' for what is a window tree).
WINDOW should be a live window, and defaults to the selected one.

This function is mainly intended to be used by
`purpose-restore-windows-1'."
  (append (list window)
	  ;; starting from 2nd sub-tree, since N sub-trees require N-1 splits
	  (cl-loop for sub-tree in (cl-cdddr tree)
		   with direction = (not (car tree))
		   collect (split-window window -5 direction))))

(defun purpose--set-size (width height &optional window pixelwise)
  "Set the size of window WINDOW to width WIDTH and height HEIGHT.
WINDOW must be a live window and defaults to the selected one.

Optional argument PIXELWISE non-nil means the width is given in pixels.
The height is never given in pixels, but in text size (number of
lines)."
  (unless (one-window-p)
    (let ((width-delta (- width (window-width window pixelwise)))
	  (height-delta (- height (window-height window))))
      (window-resize window
		     (window-resizable window width-delta
				       t nil pixelwise)
		     t nil pixelwise)
      (window-resize window
		     (window-resizable window height-delta
				       nil nil pixelwise)
		     nil nil pixelwise))))

(defun purpose--set-size-percentage (width-percentage
				     height-percentage
				     &optional window)
  (purpose--set-size
   (purpose--window-percentage-to-width width-percentage window)
   (purpose--window-percentage-to-height height-percentage window)
   window))



;;; Recursive helpers for setting/getting the layout
(defun purpose--get-layout-1 (window-tree)
  "Helper function for `purpose-get-layout'."
  (if (windowp window-tree)
      (purpose-window-params window-tree)
    (append (list (cl-first window-tree)
		  (cl-second window-tree))
	    (mapcar #'purpose--get-layout-1 (cddr window-tree)))))

(defun purpose--set-layout-1 (tree window)
  "Helper function for `purpose-set-layout'."
  (if (purpose-window-params-p tree)
      (progn
	(purpose--set-size-percentage (plist-get tree :width)
				      (plist-get tree :height)
				      window)
	(purpose-set-window-properties tree window))

    ;; this section is commented out, because it doesn't really matter
    ;; (let ((edges (second tree)))
    ;;   (purpose--set-size-percentage (- (third edges) (first edges))
    ;; 			   (- (fourth edges) (second edges))
    ;; 			   window))
    
    (let ((windows (purpose--split-window tree window)))
      (cl-loop for sub-tree in (cddr tree)
	       for window in windows
	       do (purpose--set-layout-1 sub-tree window)))))



;;; High level functions for setting/getting the layout (UI/API)
(defun purpose-get-layout ()
  "Get current layout."
  (purpose--get-layout-1 (car (window-tree))))

(defun purpose-set-layout (layout)
  "Set current layout.
LAYOUT must be a layout as returned by `purpose-get-layout'."
  (delete-other-windows)
  ;; 1. split
  ;; 2. let each window splits itself/set its size

  ;; 1. if windowp, set size+buffer
  ;; 2. split window, recurse for each window
  (if (purpose-window-params-p layout)
      (purpose-set-window-properties layout)
    (purpose--set-layout-1 layout (selected-window))))

(defun purpose-save-layout (&optional filename)
  "Save current layout to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Save layout to file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (with-temp-file (or filename purpose-default-layout-file)
    ;; "%S" - print as S-expression - this allows us to read the value with
    ;; `read' later in `purpose-load-layout'
    (insert (format "%S" (purpose-get-layout)))))

(defun purpose-load-layout (&optional filename)
  "Load layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Load layout from file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (purpose-set-layout
   (with-temp-buffer
     (insert-file-contents (or filename purpose-default-layout-file))
     (read (point-marker)))))

(provide 'purpose-layout)
;;; purpose-layout.el ends here
