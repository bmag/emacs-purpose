;;; purpose-fixes.el --- fix integration issues with other features -*- lexical-binding: t -*-

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
;; This files contains fixes that allow Purpose to work well with other
;; features, or to allow other features to work well with Purpose.

;;; Code:

(require 'purpose-switch)



;;; `compilation-next-error-function' sometimes hides the compilation buffer
;;; when Purpose is on. Solution: make the buffer's window dedicated while
;;; executing `compilation-next-error-function'

(define-purpose-compatible-advice 'compilation-next-error-function
  :around purpose--fix-compilation-next-error
  (&rest args)
  "Integrate Purpose and `compilation-next-error-function'.
Advice that prevents `compilation-next-error-function' from hiding the
compilation buffer.  This is done by ensuring that the buffer is
dedicated for the duration of the function.
This function should be advised around
`compilation-next-error-function'."
  ;; new style advice
  ((let* ((compilation-window (get-buffer-window (marker-buffer (point-marker))))
	  (old-window-dedicated-p (window-dedicated-p compilation-window)))
     (set-window-dedicated-p compilation-window t)
     (unwind-protect
	 (apply oldfun args)
       (set-window-dedicated-p compilation-window old-window-dedicated-p))))

  ;; old style advice
  ((let* ((compilation-window (get-buffer-window (marker-buffer (point-marker))))
	  (old-window-dedicated-p (window-dedicated-p compilation-window)))
     (set-window-dedicated-p compilation-window t)
     (unwind-protect
	 ad-do-it
       (set-window-dedicated-p compilation-window old-window-dedicated-p)))))



;;; Hydra's *LV* buffer should be ignored by Purpose
(defun purpose--fix-hydra-lv ()
  "Add hydra's LV buffer to Purpose's ignore list."
  (eval-after-load 'hydra
    '(add-to-list 'purpose-action-function-ignore-buffer-names "^\\*LV\\*$")))



;;; Helm's buffers should be ignored
(defun purpose--fix-helm ()
  "Add helm's buffers to Purposes's ignore list."
  (eval-after-load 'helm
    '(add-to-list 'purpose-action-function-ignore-buffer-names "^\\*Helm"))
  (eval-after-load 'helm
    '(add-to-list 'purpose-action-function-ignore-buffer-names "^\\*helm")))

;;; install fixes

(defun purpose-fix-install (&rest exclude)
  "Install fixes for integrating Purpose with other features.
EXCLUDE is a list of integrations to skip.  Known members of EXCLUDE
are:
- 'compilation-next-error-function : don't integrate with
  `compilation-next-error-function'.
- 'hydra : don't integrate with hydra
- 'helm : don't integrate with helm"
  (interactive)
  (unless (member 'compilation-next-error-function exclude)
    (purpose-advice-add 'compilation-next-error-function
			:around #'purpose--fix-compilation-next-error))
  (unless (member 'hydra exclude)
    (purpose--fix-hydra-lv))
  (unless (member 'helm exclude)
    (purpose--fix-helm)))

(provide 'purpose-fixes)
;;; purpose-fixes.el ends here
