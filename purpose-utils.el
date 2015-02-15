;;; purpose-utils.el --- Utilities

;; Author: Bar Magal (2015)
;; Package: purpose

;;; Commentary:
;; Utilities used in the Purpose package

;;; Code:

(defvar purpose-message-on-p nil
  "If non-nil, `purpose-message' will produce a message.")

(defun purpose-message (format-string &rest args)
  "Produce a message if `purpose-message-on-p' is non-nil.
The message is produced with the `message' function.  In any case,
return the formatted string. FORMAT-STRING and ARGS are passed to
`message' or `format' as is."
  (if purpose-message-on-p
      (apply #'message format-string args)
    (apply #'format format-string args)))

(provide 'purpose-utils)
