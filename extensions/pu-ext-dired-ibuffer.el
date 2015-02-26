(require 'dired)
(require 'ibuffer)
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
     (:purpose ibuffer :purpose-dedicated t :width 0.22 :height 0.4722222222222222 :edges
	       (0.7733333333333333 0.5 1.0133333333333334 0.9722222222222222)))))

(defvar pu-ext-purpose-config (purpose-conf "pu-ext-dired-ibuffer"
					    :mode-purposes
					    '((ibuffer-mode . ibuffer)
					      (dired-mode . dired))))

(defun pu-ext--setup-ibuffer ()
  (add-hook 'ibuffer-mode-hook
	    #'(lambda ()
		(ibuffer-filter-by-filename ".")))
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark " " name)))
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  ;; not sure if we want this...
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  (ibuffer-list-buffers))

(defun pu-ext-update-dired ()
  (when (and (buffer-file-name)
	     (cl-delete-if #'window-dedicated-p (purpose-windows-with-purpose 'dired)))
    (save-selected-window
      (dired (file-name-directory (buffer-file-name))))))

(defun pu-ext--setup-dired ()
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'purpose-select-buffer-hook #'pu-ext-update-dired))

(defun pu-ext-setup ()
  (interactive)
  (purpose-set-extension-configuration :pu-ext-dired-ibuffer pu-ext-purpose-config)
  (pu-ext--setup-ibuffer)
  (pu-ext--setup-dired)
  (purpose-set-window-layout pu-ext--window-layout))

(provide 'pu-ext-dired-ibuffer)
