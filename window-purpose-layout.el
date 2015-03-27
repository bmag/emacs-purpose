;;; window-purpose-layout.el --- Save and load window layout -*- lexical-binding: t -*-

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
;; This file contains function for saving and loading the entire window
;; layout and frame layout.

;;; Code:

(require 'cl-lib)
(require 'window-purpose-core)

(defcustom purpose-default-layout-file
  (concat user-emacs-directory ".purpose-layout")
  "Default file for saving/loading purpose layout."
  :group 'purpose
  :type 'file
  :package-version "1.2")

(defcustom purpose-get-extra-window-params-functions nil
  "If non-nil, this variable should be a list of functions.
This variable is used by `purpose-window-params'.  See
`purpose-window-params' for more details."
  :group 'purpose
  :type 'hook
  :package-version "1.3.50")

(defcustom purpose-set-window-properties-functions nil
  "Hook to run after calling `purpose-set-window-properties'.
Use this to set additional properties for windows as they are created,
when `purpose-set-window-layout' or `purpose-load-window-layout' is called.  Each
function in `purpose-set-window-properties-functions' is called with two
arguments: PROPERTIES and WINDOW.  PROPERTIES is the window's property
list as saved in the used layout, and WINDOW is the new window.  If
WINDOW is nil, your function should act on the selected window
instead."
  :group 'purpose
  :type 'hook
  :package-version "1.2")

(defvar purpose-mru-window-layout nil
  "Most recently used window layout.
This is the last layout used by `purpose-set-window-layout'.")

(defvar purpose-mru-frame-layout nil
  "Most recently used frame layout.
This is the last layout used by `purpose-set-frame-layout'.")



;;; window-params low-level functions
(defun purpose--window-edges-to-percentage (&optional window)
  "Convert a `window-edges' list from integers to percentages.
The percentages represent the WINDOW's edges relative to its frame's
size."
  (cl-multiple-value-bind (left top right bottom) (window-edges window)
    (let ((frame-width (frame-width (window-frame window)))
	  (frame-height (frame-height (window-frame window))))
      (list (/ left frame-width 1.0)
	    (/ top frame-height 1.0)
	    (/ right frame-width 1.0)
	    (/ bottom frame-height 1.0)))))

(defun purpose--window-width-to-percentage (&optional window)
  "Return a percentage of WINDOW's width to its frame's width.
WINDOW defaults to the selected window."
  (/ (window-total-width window) (frame-width (window-frame window)) 1.0))

(defun purpose--window-height-to-percentage (&optional window)
  "Return a percentage of WINDOW's height to its frame's height.
WINDOW defaults to the selected window."
  (/ (window-total-height window) (frame-height (window-frame window)) 1.0))

(defun purpose--window-percentage-to-width (percentage &optional window)
  "Return a window width as an integer.
The width is the PERCENTAGE of WINDOW's frame's width."
  (round (* percentage (frame-width (window-frame window)))))

(defun purpose--window-percentage-to-height (percentage &optional window)
  "Return a window height as an integer.
The height is the PERCENTAGE of WINDOW's frame's height."
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

If the variable `purpose-get-extra-window-params-functions' is non-nil,
it should be a list of functions, where each function receives a window
as an optional argument and returns a plist.  Each plist is concatenated
into the plist that `purpose-window-params' returns.  The plists returned
by `purpose-get-extra-window-params-functions' shouldn't contain any of
the keys described above (:purpose, :purpose-dedicated, :width, :height,
:edges).  If any of them does contain any of these keys, the behavior is
not defined."
  (let ((buffer (window-buffer window)))
    (apply #'append
           (list :purpose (purpose-buffer-purpose buffer)
                 :purpose-dedicated (purpose-window-purpose-dedicated-p window)
                 :width (purpose--window-width-to-percentage window)
                 :height (purpose--window-height-to-percentage window)
                 :edges (purpose--window-edges-to-percentage window))
           (mapcar #'(lambda (fn)
                       (funcall fn window))
                   purpose-get-extra-window-params-functions))))

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
	  ;; Starting from 2nd sub-tree, since N sub-trees require N-1 splits.
	  ;; Reversing, because the selected window doesn't change, so the
	  ;; first split window is actually the farthest away, and so matches
	  ;; the last sub-treeb.
	  (nreverse
	   (cl-loop for sub-tree in (cl-cdddr tree)
		    with direction = (not (car tree))
		    collect (split-window window -5 direction)))))

(defun purpose--set-size (width height &optional window)
  "Set the size of window WINDOW to width WIDTH and height HEIGHT.
WINDOW must be a live window and defaults to the selected one."
  (unless (one-window-p)
    (let ((width-delta (- width (window-total-width window)))
	  (height-delta (- height (window-total-height window))))
      (window-resize window
		     (window-resizable window width-delta t nil)
		     t nil)
      (window-resize window
		     (window-resizable window height-delta nil nil)
		     nil nil))))

(defun purpose--set-size-percentage (width-percentage
				     height-percentage
				     &optional window)
  (purpose--set-size
   (purpose--window-percentage-to-width width-percentage window)
   (purpose--window-percentage-to-height height-percentage window)
   window))



;;; Recursive helpers for setting/getting the layout
(defun purpose--get-window-layout-1 (window-tree)
  "Helper function for `purpose-get-window-layout'."
  (if (windowp window-tree)
      (purpose-window-params window-tree)
    (append (list (cl-first window-tree)
		  (cl-second window-tree))
	    (mapcar #'purpose--get-window-layout-1 (cddr window-tree)))))

(defun purpose--set-window-layout-1 (tree window)
  "Helper function for `purpose-set-window-layout'."
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
	       do (purpose--set-window-layout-1 sub-tree window)))))



;;; High level functions for setting/getting the layout (UI/API)

;;; get/set/load/save window layout

(defun purpose-get-window-layout (&optional frame)
  "Get window layout of FRAME.
FRAME defaults to the selected frame."
  (purpose--get-window-layout-1 (car (window-tree frame))))

(defun purpose-set-window-layout (layout &optional frame norecord)
  "Set LAYOUT as FRAME's window layout.
FRAME defaults to the selected frame.
LAYOUT must be a layout as returned by `purpose-get-window-layout'.
Unless NORECORD is non-nil, this function sets LAYOUT as the value of
`purpose-mru-window-layout'.
This function doesn't change the selected frame (uses
`with-selected-frame' internally)."
  (with-selected-frame (or frame (selected-frame))
    (delete-other-windows)
    ;; ensure that the remaining window is not dedicated, otherwise we won't be
    ;; able to change its buffer, and an error will be signaled (issue #38)
    (set-window-dedicated-p nil nil)
    ;; 1. split
    ;; 2. let each window splits itself/set its size

    ;; 1. if windowp, set size+buffer
    ;; 2. split window, recurse for each window
    (if (purpose-window-params-p layout)
	(purpose-set-window-properties layout)
      (purpose--set-window-layout-1 layout (selected-window)))
    (unless norecord
      (setq purpose-mru-window-layout layout))))

(defun purpose-save-window-layout (&optional filename)
  "Save window layout of current frame to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Save window layout to file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (with-temp-file (or filename purpose-default-layout-file)
    ;; "%S" - print as S-expression - this allows us to read the value with
    ;; `read' later in `purpose-load-window-layout'
    (insert (format "%S" (purpose-get-window-layout)))))

(defun purpose-load-window-layout (&optional filename)
  "Load window layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Load window layout from file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (purpose-set-window-layout
   (with-temp-buffer
     (insert-file-contents (or filename purpose-default-layout-file))
     (read (point-marker)))))

(defun purpose-reset-window-layout ()
  "Load window layout from `purpose-mru-window-layout'.
If `purpose-mru-window-layout' is nil, do nothing."
  (interactive)
  (when purpose-mru-window-layout
    (purpose-set-window-layout purpose-mru-window-layout)))

;;; get/set/load/save frame layout

(defun purpose-get-frame-layout ()
  "Return Emacs' frame layout.
The frame layout is a list of all live frames' window layouts. Each
window-layout is a window-layout as returned by
`purpose-get-window-layout'."
  (mapcar #'purpose-get-window-layout (frame-list)))

(defun purpose-set-frame-layout (layout &optional norecord)
  "Set LAYOUT as Emacs' frame layout.
LAYOUT must be a layout as returned by `purpose-get-frame-layout'.
Unless NORECORD is non-nil, this function sets LAYOUT as the value of
`purpose-mru-frame-layout'.
This function deletes all existing frames and creates frames as
specified by LAYOUT."
  (delete-other-frames)
  (purpose-set-window-layout (car layout) nil t)
  (dolist (window-layout (cdr layout))
    (purpose-set-window-layout window-layout (make-frame) t))
  (unless norecord
    (setq purpose-mru-frame-layout layout)))

(defun purpose-save-frame-layout (&optional filename)
  "Save frame layout of Emacs to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Save frame layout to file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (with-temp-file (or filename purpose-default-layout-file)
    ;; "%S" - print as S-expression - this allows us to read the value with
    ;; `read' later in `purpose-load-window-layout'
    (insert (format "%S" (purpose-get-frame-layout)))))

(defun purpose-load-frame-layout (&optional filename)
  "Load frame layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name "[PU] Load frame layout from file: "
			 (file-name-directory purpose-default-layout-file)
			 nil nil
			 (file-name-nondirectory purpose-default-layout-file))))
  (purpose-set-frame-layout
   (with-temp-buffer
     (insert-file-contents (or filename purpose-default-layout-file))
     (read (point-marker)))))

(defun purpose-reset-frame-layout ()
  "Load frame layout from `purpose-mru-frame-layout'.
If `purpose-mru-frame-layout' is nil, do nothing."
  (interactive)
  (when purpose-mru-frame-layout
    (purpose-set-frame-layout purpose-mru-frame-layout)))



;;; Other

(defun purpose-delete-non-dedicated-windows ()
  "Delete all windows that aren't dedicated to their purpose or buffer."
  (interactive)
  (mapc #'(lambda (window)
	    (when (and (window-deletable-p window)
		       (not (window-dedicated-p window))
		       (not (purpose-window-purpose-dedicated-p window)))
	      (delete-window window)))
	(window-list)))

(defun purpose-set-window-purpose (purpose &optional dont-dedicate)
  "Set window's purpose to PURPOSE, and dedicate it.
With prefix argument (DONT-DEDICATE is non-nil), don't dedicate the
window.  Changing the window's purpose is done by displaying a buffer of
the right purpose in it, or creating a dummy buffer."
  (interactive "SPurpose: \nP")
  (purpose--set-window-buffer purpose)
  (unless dont-dedicate
    (purpose-set-window-purpose-dedicated-p nil t)))

(defun purpose--delete-window-at (window-getter &optional frame)
  "Delete window returned by WINDOW-GETTER.
WINDOW-GETTER should be a function that takes one argument - FRAME."
  (let ((window (funcall window-getter frame)))
    (if window
	(delete-window window)
      (user-error "Couldn't find window."))))

(defun purpose-delete-window-at-top (&optional frame)
  "Delete window at top.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-top-window frame))

(defun purpose-delete-window-at-bottom (&optional frame)
  "Delete window at bottom.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-bottom-window frame))

(defun purpose-delete-window-at-left (&optional frame)
  "Delete window at left.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-left-window frame))

(defun purpose-delete-window-at-right (&optional frame)
  "Delete window at right.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-right-window frame))

(provide 'window-purpose-layout)
;;; window-purpose-layout.el ends here
