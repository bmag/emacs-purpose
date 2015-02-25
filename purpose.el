;;; purpose.el --- Purpose-based window management for Emacs -*- lexical-binding: t -*-

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 1.2.50
;; Keywords: frames
;; Homepage: https://github.com/bmag/emacs-purpose
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (let-alist "1.0.3"))

;;; Commentary:

;; ---------------------------------------------------------------------
;; Full information can be found on GitHub:
;; https://github.com/bmag/emacs-purpose/wiki
;; ---------------------------------------------------------------------

;; Purpose is a package that introduces the concept of a "purpose" for
;; windows and buffers, and then helps you maintain a robust window
;; layout easily.

;; Installation and Setup:
;; Install Purpose from MELPA, or download it manually from GitHub. If
;; you download manually, add these lines to your init file:
;;    (add-to-list 'load-path "/path/to/purpose")
;;    (require 'purpose)
;; To activate Purpose at start-up, add this line to your init file:
;;    (purpose-mode)

;; Purpose Configuration:
;; Customize `purpose-user-mode-purposes', `purpose-user-name-purposes',
;; `purpose-user-regexp-purposes' and
;; `purpose-use-default-configuration'.

;; Basic Usage:
;; 1. Load/Save window/frame layout (see `purpose-load-window-layout',
;;    `purpose-save-window-layout', etc.)
;; 2. Use regular switch-buffer functions - they will not mess your
;;    window layout (Purpose overrides them).
;; 3. If you don't want a window's purpose/buffer to change, dedicate
;;    the window:
;;    C-c , d: `purpose-toggle-window-purpose-dedicated'
;;    C-c , D: `purpose-toggle-window-buffer-dedicated'
;; 4. To use a switch-buffer function that ignores Purpose, prefix it
;;    with C-u. For example, [C-u C-x b] calls
;;    `switch-buffer-without-purpose'.

;;; Code:

(require 'purpose-utils)
(require 'purpose-configuration)
(require 'purpose-core)
(require 'purpose-layout)
(require 'purpose-switch)
(require 'purpose-prefix-overload)
(require 'purpose-fixes)

(defconst purpose-version "1.2.50"
  "Purpose's version.")


;;; Commands for using Purpose-less behavior
(fset 'find-file-without-purpose
      (without-purpose-command #'ido-find-file))

(fset 'find-file-other-window-without-purpose
      (without-purpose-command #'ido-find-file-other-window))

(fset 'find-file-other-frame-without-purpose
      (without-purpose-command #'ido-find-file-other-frame))

(fset 'switch-buffer-without-purpose
      (without-purpose-command #'ido-switch-buffer))

(fset 'switch-buffer-other-window-without-purpose
      (without-purpose-command #'ido-switch-buffer-other-window))

(fset 'switch-buffer-other-frame-without-purpose
      (without-purpose-command #'ido-switch-buffer-other-frame))


;;; Overloaded commands: (C-u to get original Purpose-less behavior)
(define-purpose-prefix-overload purpose-find-file-overload
  '(ido-find-file find-file-without-purpose))

(define-purpose-prefix-overload purpose-find-file-other-window-overload
  '(ido-find-file-other-window find-file-other-window-without-purpose))

(define-purpose-prefix-overload purpose-find-file-other-frame-overload
  '(ido-find-file-other-frame find-file-other-frame-without-purpose))

(define-purpose-prefix-overload purpose-switch-buffer-overload
  '(ido-switch-buffer
    switch-buffer-without-purpose
    purpose-switch-buffer-with-purpose))

(define-purpose-prefix-overload purpose-switch-buffer-other-window-overload
  '(ido-switch-buffer-other-window
    switch-buffer-other-window-without-purpose
    purpose-switch-buffer-with-purpose-other-window))

(define-purpose-prefix-overload purpose-switch-buffer-other-frame-overload
  '(ido-switch-buffer-other-frame
    switch-buffer-other-frame-without-purpose
    purpose-switch-buffer-with-purpose-other-frame))



(defvar purpose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-f") #'purpose-find-file-overload)
    (define-key map (kbd "C-x 4 f") #'purpose-find-file-other-window-overload)
    (define-key map (kbd "C-x 4 C-f") #'purpose-find-file-other-window-overload)
    (define-key map (kbd "C-x 5 f") #'purpose-find-file-other-frame-overload)
    (define-key map (kbd "C-x 5 C-f") #'purpose-find-file-other-frame-overload)
    
    (define-key map (kbd "C-x b") #'purpose-switch-buffer-overload)
    (define-key map (kbd "C-x 4 b") #'purpose-switch-buffer-other-window-overload)
    (define-key map (kbd "C-x 5 b") #'purpose-switch-buffer-other-frame-overload)

    ;; Helpful for quitting temporary windows. Close in meaning to
    ;; `kill-buffer', so we map it to a close key ("C-x j" is close to
    ;; "C-x k")
    (define-key map (kbd "C-x j") #'quit-window)

    ;; We use "C-c ," for compatibility with key-binding conventions
    (define-key map (kbd "C-c ,") 'purpose-mode-prefix-map)
    (define-prefix-command 'purpose-mode-prefix-map)
    (define-key purpose-mode-prefix-map (kbd "o") #'purpose-switch-buffer)
    (define-key purpose-mode-prefix-map
      (kbd "[") #'purpose-switch-buffer-other-frame)
    (define-key purpose-mode-prefix-map
      (kbd "p") #'purpose-switch-buffer-other-window)
    (define-key purpose-mode-prefix-map
      (kbd "d") #'purpose-toggle-window-purpose-dedicated)
    (define-key purpose-mode-prefix-map
      (kbd "D") #'purpose-toggle-window-buffer-dedicated)

    map)
  "Keymap for Purpose mode.")

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Menu-Keymaps.html#Menu-Keymaps
;; (defvar purpose-menu-bar-map (make-sparse-keymap "Purpose"))

(defun purpose--modeline-string ()
  "Return the presentation of a window's purpose for display in the
modeline.  The string returned has two forms.  For example, if window's
purpose is 'edit: If (purpose-window-purpose-dedicated-p), return
\"[edit!]\", otherwise return \"[edit]\"."
  (format " [%s%s]"
	  (purpose-window-purpose)
	  (if (purpose-window-purpose-dedicated-p) "!" "")))

(defun purpose--add-advices ()
  "Add all advices needed for Purpose to work.
This function is called when `purpose-mode' is activated."
  (purpose-advice-add 'switch-to-buffer :around
		      #'purpose-switch-to-buffer-advice)
  (purpose-advice-add 'switch-to-buffer-other-window :around
		      #'purpose-switch-to-buffer-other-window-advice)
  (purpose-advice-add 'switch-to-buffer-other-frame :around
		      #'purpose-switch-to-buffer-other-frame-advice)
  (purpose-advice-add 'pop-to-buffer :around
		      #'purpose-pop-to-buffer-advice)
  (purpose-advice-add 'pop-to-buffer-same-window :around
		      #'purpose-pop-to-buffer-same-window-advice)
  (purpose-advice-add 'display-buffer :around
		      #'purpose-display-buffer-advice))

(defun purpose--remove-advices ()
  "Remove all advices needed for Purpose to work.
This function is called when `purpose-mode' is deactivated."
  (purpose-advice-remove 'switch-to-buffer :around
			 #'purpose-switch-to-buffer-advice)
  (purpose-advice-remove 'switch-to-buffer-other-window :around
			 #'purpose-switch-to-buffer-other-window-advice)
  (purpose-advice-remove 'switch-to-buffer-other-frame :around
			 #'purpose-switch-to-buffer-other-frame-advice)
  (purpose-advice-remove 'pop-to-buffer :around
			 #'purpose-pop-to-buffer-advice)
  (purpose-advice-remove 'pop-to-buffer-same-window :around
			 #'purpose-pop-to-buffer-same-window-advice)
  (purpose-advice-remove 'display-buffer :around
			 #'purpose-display-buffer-advice))

(define-minor-mode purpose-mode
  nil :global t :lighter (:eval (purpose--modeline-string))
  (if purpose-mode
      (progn
	(purpose--add-advices)
	(setq display-buffer-overriding-action
	      '(purpose--action-function . nil))
	(setq purpose--active-p t)
	(purpose-fix-install))
    (purpose--remove-advices)
    (setq purpose--active-p nil)))

(provide 'purpose)
;;; purpose.el ends here
