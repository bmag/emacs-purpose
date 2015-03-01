(require 'dired)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'purpose)

(defvar pu-ext--window-layout
  '(nil
    (0 0 152 35)
    (:purpose edit :purpose-dedicated t :width 0.7466666666666667 :height 0.9722222222222222 :edges
	      (0.0 0.0 0.7733333333333333 0.9722222222222222))
    (t
     (116 0 152 35)
     (:purpose dired :purpose-dedicated t :width 0.21333333333333335 :height 0.5 :edges
	       (0.7733333333333333 0.0 1.0133333333333334 0.5))
     (:purpose buffers :purpose-dedicated t :width 0.22 :height 0.4722222222222222 :edges
	       (0.7733333333333333 0.5 1.0133333333333334 0.9722222222222222))))
    "Window layout for pu-ext-dired-ibuffer.
Has a main 'edit window, and two side windows - 'dired and 'buffers.
All windows are purpose-dedicated.")

;; the name arg ("pu-ext-dired-ibuffer") is necessary for Emacs 24.3 and older
(defvar pu-ext-purpose-config (purpose-conf "pu-ext-dired-ibuffer"
					    :mode-purposes
					    '((ibuffer-mode . buffers)
					      (dired-mode . dired))))

(define-ibuffer-filter pu-ext-ibuffer-files-only
    "Display only buffers that are bound to files."
  ()
  (buffer-file-name buf))

(defun pu-ext--setup-ibuffer ()
  "Set up ibuffer settings."
  (add-hook 'ibuffer-mode-hook
  	    #'(lambda ()
  		(ibuffer-filter-by-pu-ext-ibuffer-files-only nil)))
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark " " name)))
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  ;; not sure if we want this...
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  (when (get-buffer "*Ibuffer*")
    (kill-buffer "*Ibuffer*"))
  (ibuffer-list-buffers))

(defun pu-ext-update-dired ()
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

(defun pu-ext--setup-dired ()
  "Setup dired settings."
  (add-hook 'purpose-select-buffer-hook #'pu-ext-update-dired))

(defun pu-ext-setup ()
  "Setup pu-ext-dired-ibuffer.
This setup includes 3 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'."
  (interactive)
  (purpose-set-extension-configuration :pu-ext-dired-ibuffer pu-ext-purpose-config)
  (pu-ext--setup-ibuffer)
  (pu-ext--setup-dired)
  (purpose-set-window-layout pu-ext--window-layout))

(provide 'pu-ext-dired-ibuffer)
