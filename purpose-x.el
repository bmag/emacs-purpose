;;; purpose-x.el --- Extensions for Purpose -*- lexical-binding: t -*-

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
;; File containing extensions for Purpose.
;; Extensions included:
;; - code1: 4-window display: main edit window, `dired' side window,
;;   `ibuffer' side window and `imenu-list' side window.
;; - magit: purpose configurations for magit.
;; - golden-ratio: make `golden-ratio-mode' work correctly with Purpose.

;;; Code:

(require 'purpose)

;;; --- purpose-x-code1 ---
;;; purpose-x-code1 extension creates a 4-window display:
;;; 1 main window for code buffers (purpose 'edit)
;;; 3 sub windows:
;;; - dired window: show directory of current buffer
;;; - ibuffer window: show currently open files
;;; - imenu-list window: show imenu of current buffer

(require 'dired)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'imenu-list)

(defvar purpose-x-code1--window-layout
  '(nil
    (0 0 152 35)
    (t
     (0 0 29 35)
     (:purpose dired :purpose-dedicated t :width 0.16 :height 0.5 :edges
	       (0.0 0.0 0.19333333333333333 0.5))
     (:purpose buffers :purpose-dedicated t :width 0.16 :height 0.4722222222222222 :edges
	       (0.0 0.5 0.19333333333333333 0.9722222222222222)))
    (:purpose edit :purpose-dedicated t :width 0.6 :height 0.9722222222222222 :edges
	      (0.19333333333333333 0.0 0.8266666666666667 0.9722222222222222))
    (:purpose ilist :purpose-dedicated t :width 0.15333333333333332 :height 0.9722222222222222 :edges
	      (0.8266666666666667 0.0 1.0133333333333334 0.9722222222222222)))
  "Window layout for purpose-x-code1-dired-ibuffer.
Has a main 'edit window, and two side windows - 'dired and 'buffers.
All windows are purpose-dedicated.")

;; the name arg ("purpose-x-code1") is necessary for Emacs 24.3 and older
(defvar purpose-x-code1-purpose-config (purpose-conf "purpose-x-code1"
					    :mode-purposes
					    '((ibuffer-mode . buffers)
					      (dired-mode . dired)
					      (imenu-list-major-mode . ilist))))

(define-ibuffer-filter purpose-x-code1-ibuffer-files-only
    "Display only buffers that are bound to files."
  ()
  (buffer-file-name buf))

(defun purpose-x-code1--setup-ibuffer ()
  "Set up ibuffer settings."
  (add-hook 'ibuffer-mode-hook
  	    #'(lambda ()
  		(ibuffer-filter-by-purpose-x-code1-ibuffer-files-only nil)))
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark " " name)))
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  ;; not sure if we want this...
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  (when (get-buffer "*Ibuffer*")
    (kill-buffer "*Ibuffer*"))
  (ibuffer-list-buffers))

(defun purpose-x-code1--unset-ibuffer ()
  "Unset ibuffer settings."
  (remove-hook 'ibuffer-mode-hook
	       #'(lambda ()
		   (ibuffer-filter-by-purpose-x-code1-ibuffer-files-only nil)))
  (remove-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark modified read-only " "
				(name 18 18 :left :elide)
				" "
				(size 9 -1 :right)
				" "
				(mode 16 16 :left :elide)
				" " filename-and-process)
			  (mark " "
				(name 16 -1)
				" " filename)))
  (setq ibuffer-display-summary t)
  (setq ibuffer-use-header-line t))

(defun purpose-x-code1-update-dired ()
  "Update free dired window with current buffer's directory.
If a non-buffer-dedicated window with purpose 'dired exists, display
the directory of the current buffer in that window, using `dired'.
If there is no window available, do nothing.
If current buffer doesn't have a filename, do nothing."
  (when (and (buffer-file-name)
	     (cl-delete-if #'window-dedicated-p (purpose-windows-with-purpose 'dired)))
    (save-selected-window
      (dired (file-name-directory (buffer-file-name)))
      (when (fboundp 'dired-hide-details-mode)
	(dired-hide-details-mode)))))

(defun purpose-x-code1--setup-dired ()
  "Setup dired settings."
  (add-hook 'purpose-select-buffer-hook #'purpose-x-code1-update-dired))

(defun purpose-x-code1--unset-dired ()
  "Unset dired settings."
  (remove-hook 'purpose-select-buffer-hook #'purpose-x-code1-update-dired))

(defun purpose-x-code1--setup-imenu-list ()
  "Setup imenu-list settings."
  (add-hook 'purpose-select-buffer-hook #'imenu-list-update-safe)
  (imenu-list-minor-mode 1))

(defun purpose-x-code1--unset-imenu-list ()
  "Unset imenu-list settings."
  (remove-hook 'purpose-select-buffer-hook #'imenu-list-update-safe)
  (imenu-list-minor-mode -1))

;;;###autoload
(defun purpose-x-code1-setup ()
  "Setup purpose-x-code1.
This setup includes 4 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'.
4. dedicated 'ilist window.  This window shows the current buffer's
imenu."
  (interactive)
  (purpose-set-extension-configuration :purpose-x-code1 purpose-x-code1-purpose-config)
  (purpose-x-code1--setup-ibuffer)
  (purpose-x-code1--setup-dired)
  (purpose-x-code1--setup-imenu-list)
  (purpose-set-window-layout purpose-x-code1--window-layout))

(defun purpose-x-code1-unset ()
  "Unset purpose-x-code1."
  (interactive)
  (purpose-del-extension-configuration :purpose-x-code1)
  (purpose-x-code1--unset-ibuffer)
  (purpose-x-code1--unset-dired)
  (purpose-x-code1--unset-imenu-list))

;;; --- purpose-x-code1 ends here ---



;;; --- purpose-x-magit ---
;;; purpose-x-magit extension provides purpose configuration for magit.
;;; Two configurations available:
;;; - `purpose-x-magit-single-conf': all magit windows have the same purpose
;;;                                  ('magit)
;;; - `purpose-x-magit-multi-conf': each magit major-mode has a seperate
;;;                                 purpose ('magit-status, 'magit-diff, ...)
;;; Use these commands to enable and disable magit's purpose configurations:
;;; - `purpose-x-magit-single-on'
;;; - `purpose-x-magit-multi-on'
;;; - `purpose-x-magit-off'

(defvar purpose-x-magit-single-conf
  (purpose-conf "magit-single"
		:regexp-purposes '(("^\\*magit" . magit)))
  "Configuration that gives each magit major-mode the same purpose.")

(defvar purpose-x-magit-multi-conf
  (purpose-conf
   "magit-multi"
   :mode-purposes '((magit-diff-mode . magit-diff)
		    (magit-status-mode . magit-status)
		    (magit-log-mode . magit-log)
		    (magit-commit-mode . magit-commit)
		    (magit-cherry-mode . magit-cherry)
		    (magit-branch-manager-mode . magit-branch-manager)
		    (magit-process-mode . magit-process)
		    (magit-reflog-mode . magit-reflog)
		    (magit-wazzup-mode . magit-wazzup)))
  "Configuration that gives each magit major-mode its own purpose.")

;;;###autoload
(defun purpose-x-magit-single-on ()
  "Turn on magit-single purpose configuration."
  (interactive)
  (purpose-set-extension-configuration :magit purpose-x-magit-single-conf))

;;;###autoload
(defun purpose-x-magit-multi-on ()
  "Turn on magit-multi purpose configuration."
  (interactive)
  (purpose-set-extension-configuration :magit purpose-x-magit-multi-conf))

(defun purpose-x-magit-off ()
  "Turn off magit purpose configuration (single or multi)."
  (interactive)
  (purpose-del-extension-configuration :magit))

;;; --- purpose-x-magit ends here ---



;;; --- purpose-x-golden-ration ---
;;; Make `purpose-mode' and `golden-ratio-mode' work together properly.
;;; Basically, this adds a hook to `purpose-select-buffer-hook' so
;;; `golden-ratio' is called when a buffer is selected via Purpose.

(defun purpose-x-sync-golden-ratio ()
  "Add/remove `golden-ratio' to `purpose-select-buffer-hook'.
Add `golden-ratio' at the end of `purpose-select-buffer-hook' if
`golden-ratio-mode' is on, otherwise remove it."
  (if golden-ratio-mode
      (add-hook 'purpose-select-buffer-hook #'golden-ratio t)
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

;;;###autoload
(defun purpose-x-golden-ratio-setup ()
  "Make `golden-ratio-mode' aware of `purpose-mode'."
  (interactive)
  (add-hook 'golden-ratio-mode-hook #'purpose-x-sync-golden-ratio)
  (when (and (boundp 'golden-ratio-mode) golden-ratio-mode)
    (add-hook 'purpose-select-buffer-hook #'golden-ratio t)))

(defun purpose-x-golden-ratio-unset ()
  "Make `golden-ratio-mode' forget about `purpose-mode'."
  (interactive)
  (remove-hook 'golden-ratio-mode-hook #'purpose-x-sync-golden-ratio)
  (when (and (boundp 'golden-ratio-mode) golden-ratio-mode)
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

;;; --- purpose-x-golden-ration ends here ---



;;; --- purpose-x-popup-switcher ---
;;; A command for combinining `popup-switcher' with
;;; `purpose-switch-buffer-with-purpose'.
;;; This requires package `popup-switcher'

(when (require 'popup-switcher nil t)
  (defun purpose-psw-switch-buffer-with-purpose ()
    (interactive)
    (psw-switcher :items-list (purpose-buffers-with-purpose
			       (purpose-buffer-purpose (current-buffer)))
		  :item-name-getter #'buffer-name
		  :switcher #'purpose-switch-buffer)))

;;; --- purpose-x-popup-switcher ends here ---


(provide 'purpose-x)
;;; purpose-x.el ends here
