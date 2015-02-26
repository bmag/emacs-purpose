;;; purpose-fixes.el --- fix integration issues with other features -*- lexical-binding: t -*-

;; Author: Bar Magal (2015)
;; Package: purpose

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
    '(add-to-list 'purpose-action-function-ignore-buffer-names "*LV*")))


;;; install fixes

(defun purpose-fix-install (&rest exclude)
  "Install fixes for integrating Purpose with other features.
EXCLUDE is a list of integrations to skip.  Known members of EXCLUDE
are:
- 'compilation-next-error-function : don't integrate with
  `compilation-next-error-function'.
- 'hydra : don't integrate with hydra"
  (interactive)
  (unless (member 'compilation-next-error-function exclude)
    (purpose-advice-add 'compilation-next-error-function
			:around #'purpose--fix-compilation-next-error))
  (unless (member 'hydra exclude)
    (purpose--fix-hydra-lv)))

(provide 'purpose-fixes)
;;; purpose-fixes.el ends here
