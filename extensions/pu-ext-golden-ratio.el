;;; pu-ext-golden-ratio.el --- Integrate Purpose and golden-ratio

;;; Commentary:
;; Make purpose-mode and golden-ratio-mode work together properly.
;; purpose-mode: https://github.com/bmag/emacs-purpose
;; golden-ratio-mode: https://github.com/roman/golden-ratio.el

;;; Installation:
;; (add-to-list 'load-path "/path/to/pu-ext-golden-ratio-dir")
;; (require 'pu-ext-golden-ratio)
;; (pu-ext-golden-ratio-setup)

;;; Code:

(defun pu-ext-sync-golden-ratio-with-purpose ()
  "Add/remove `golden-ratio' to `purpose-select-buffer-hook'.
Add `golden-ratio' at the end of `purpose-select-buffer-hook' if
`golden-ratio-mode' is on, otherwise remove it."
  (if golden-ratio-mode
      (add-hook 'purpose-select-buffer-hook #'golden-ratio t)
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

(defun pu-ext-golden-ratio-setup ()
  "Make `golden-ratio-mode' aware of `purpose-mode'."
  (interactive)
  (add-hook 'golden-ratio-mode-hook #'pu-ext-sync-golden-ratio-with-purpose)
  (when (and (boundp 'golden-ratio-mode) golden-ratio-mode)
    (add-hook 'purpose-select-buffer-hook #'golden-ratio t)))

(defun pu-ext-golden-ratio-unset ()
  "Make `golden-ratio-mode' forget about `purpose-mode'."
  (interactive)
  (remove-hook 'golden-ratio-mode-hook #'pu-ext-sync-golden-ratio-with-purpose)
  (when (and (boundp 'golden-ratio-mode) (not golden-ratio-mode))
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

(provide 'pu-ext-golden-ratio)
;;; pu-ext-golden-ratio.el ends here
