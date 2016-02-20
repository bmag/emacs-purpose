;;; window-purpose-actions.el --- Missing actions for display-buffer -*- lexical-binding: t -*-

;; Copyright (C) 2016 Bar Magal

;; Author: Bar Magal
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
;; Emacs doesn't provide many display actions and display functions to work with
;; `display-buffer' and `display-buffer-alist'. The result is many
;; implementations of similar window-creation code, some worse than others.
;; Here, we provide some of the missing display actions and display functions.

;; Available utilities:
;; - `purpose-fit-window-to-buffer-horizontally'

;; Available display functions:
;; - `purpose-display-split-window'
;; - `purpose-display-split-frame'

;; Available actions:
;; - `purpose-split-above-action'
;; - `purpose-split-below-action'
;; - `purpose-split-left-action'
;; - `purpose-split-right-action'
;; - `purpose-split-frame-above-action'
;; - `purpose-split-frame-below-action'
;; - `purpose-split-frame-left-action'
;; - `purpose-split-frame-right-action'

;;; Code:

(defun purpose-fit-window-to-buffer-horizontally (&rest args)
  "Fit window to buffer horizontally.
This is the same as `fit-window-to-buffer', but only allows to
fit the window horizontally, regardless of the value of
`fit-window-to-buffer-horizontally'.

This function can be called with the same arguments as
`fit-window-to-buffer'."
  (let ((fit-window-to-buffer-horizontally 'only))
    (apply #'fit-window-to-buffer args)))

(defun purpose-display-split-window (buffer alist &optional window)
  "Split selected window and display BUFFER in the new window.
BUFFER is a buffer to display, ALIST is an alist as described in
`display-buffer'.

If ALIST contains a `split-side' entry, it specifies in what side
to split the selected window. `split-side' should be a valid
value for the SIDE argument of `split-window'. If there is no
`split-side' entry, then the default side of `split-window' is
used.

The size of the new window can be specified by including a
`window-width' or `window-height' entry in ALIST. Their meaning
is described in `display-buffer'.

If WINDOW is non-nil, it specifies a window to split instead of
the selected window.

If ALIST contains a `split-window' entry, that entry specifies a
window to split instead of the selected window. The
`split-window' entry has a lower priority than the WINDOW
argument."
  (let* ((side (purpose-alist-get 'split-side alist))
         (window (or window
                     (purpose-alist-get 'split-window alist)))
         (new-window (split-window window nil side)))
    (purpose-change-buffer buffer new-window 'window alist)))

(defun purpose-display-split-frame (buffer alist &optional frame)
  "Split selected frame and display BUFFER in the new window.
BUFFER is a buffer to display, ALIST is an alist as described in
`display-buffer'.

If ALIST contains `split-side', `window-width' or `window-height'
entries, they have the same meaning as described in
`purpose-display-split-window'.

If FRAME is non-nil, it specifies a frame to split instead of
the selected frame.

If ALIST contains a `split-frame' entry, that entry specifies a
frame to split instead of the selected frame. The
`split-frame' entry has a lower priority than the FRAME
argument."
  (let ((frame (or frame
                   (purpose-alist-get 'split-frame alist))))
    (purpose-display-split-window buffer alist (frame-root-window frame))))

(defvar purpose-split-above-action
  '(purpose-display-split-window
    (split-side . above)
    (window-height . fit-window-to-buffer))
  "Action to display a buffer in a automatically resized window above current one.
Example:
 (pop-to-buffer \"foo\" purpose-split-above-action)")

(defvar purpose-split-below-action
  '(purpose-display-split-window
    (split-side . below)
    (window-height . fit-window-to-buffer))
  "Action to display a buffer in a automatically resized window below current one.
Example:
 (pop-to-buffer \"foo\" purpose-split-below-action)")

(defvar purpose-split-left-action
  '(purpose-display-split-window
    (split-side . left)
    (window-width . purpose-fit-window-to-buffer-horizontally))
  "Action to display a buffer in a automatically resized window left of current one.
Example:
 (pop-to-buffer \"foo\" purpose-split-left-action)")

(defvar purpose-split-right-action
  '(purpose-display-split-window
    (split-side . right)
    (window-width . purpose-fit-window-to-buffer-horizontally))
  "Action to display a buffer in a automatically resized window right of current one.
Example:
 (pop-to-buffer \"foo\" purpose-split-right-action)")

(defvar purpose-split-frame-above-action
  '(purpose-display-split-frame
    (split-side . above)
    (window-height . fit-window-to-buffer))
  "Action to display a buffer in the top of the current frame.
The new window is automatically resized.
Example:
 (pop-to-buffer \"foo\" purpose-split-frame-above-action)")

(defvar purpose-split-frame-below-action
  '(purpose-display-split-frame
    (split-side . below)
    (window-height . fit-window-to-buffer))
  "Action to display a buffer in the bottom of the current frame.
The new window is automatically resized.
Example:
 (pop-to-buffer \"foo\" purpose-split-frame-below-action)")

(defvar purpose-split-frame-left-action
  '(purpose-display-split-frame
    (split-side . left)
    (window-width . purpose-fit-window-to-buffer-horizontally))
  "Action to display a buffer in the left of the current frame.
The new window is automatically resized.
Example:
 (pop-to-buffer \"foo\" purpose-split-frame-left-action)")

(defvar purpose-split-frame-right-action
  '(purpose-display-split-frame
    (split-side . right)
    (window-width . purpose-fit-window-to-buffer-horizontally))
  "Action to display a buffer in the right of the current frame.
The new window is automatically resized.
Example:
 (pop-to-buffer \"foo\" purpose-split-frame-right-action)")

(provide 'window-purpose-actions)
;;; window-purpose-actions.el ends here
