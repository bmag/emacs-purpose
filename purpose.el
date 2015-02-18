;;; purpose.el --- Handle buffers and windows by their purposes

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 1.1.50
;; Keywords: frames
;; Homepage: https://github.com/bmag/emacs-purpose
;; Package-Requires: ((cl-lib "0.5") (let-alist "1.0.3"))

;;; Commentary:

;; Purpose is a package that introduces the concept of a "purpose" for
;; windows and buffers, and then helps you maintain a robust window
;; layout easily. Purpose is intended to help both regular users and
;; developers who want Emacs to have a more IDE-like behavior.

;; More information can be found on GitHub: https://github.com/bmag/emacs-purpose/wiki

;; Installation:
;; Download Purpose's source files and put them in your `load-path'.
;; Purpoes is available from MELPA, so the best way to do this is
;; with Emacs' package manager.
;; Next, add this line to your init file:
;;    (purpose-mode)
;; If you prefer to download manually, you need to add these lines as
;; well (before calling `purpose-mode'):
;;    (add-to-list 'load-path "/path/to/purpose")
;;    (require 'purpose)

;; Typical Usage (Regular User)
;; 1. Turn Purpose on (`purpose-mode').
;; 2. Configure which purposes you want your windows to have (see
;;    purpose-configuration.el).
;; 3. Arrange your window layout as you want it to be. Any window which
;;    you want to dedicate to a specific purpose (so it won't be used
;;    for other purposes), you shuld dedicate with
;;    `purpose-toggle-window-purpose-dedicated'.
;; 4. Purpose uses advice, so Emacs uses purpose-aware commands instead
;;    of the original commands when you need to change buffers. (e.g.
;;    `purpose-switch-buffer' instead of `switch-to-buffer'). This will
;;    open your buffers in the correct windows.
;; - To save your layout, or load a previously saved layout, use
;;    `purpose-save-window-layout', `purpose-load-window-layout',
;;    `purpose-save-frame-layout' and `purpose-load-frame-layout'. You
;;    can load a saved layout and skip phases 1 and 2, of course.

;; Important Features:
;; - Configurable: Configure how Purpose decides what's your buffer's
;;    purpose. Note that the window's purpose is determined by its
;;    buffer.
;; - Persistent Window Layout: You can save and load your window layout
;;    between sessions by using `purpose-save-window-layout',
;;    `purpose-load-window-layout', `purpose-save-frame-layout' and
;;    `purpose-load-frame-layout'.
;; - Purpose-Aware Buffer Switching: Purpose uses advices (overrides)
;;    `display-buffer-overriding-action' in order to make Emacs' buffer
;;    switching functions "purpose-aware".
;; - Developer-Friendly: Purpose has hooks and an API that should make
;;    it easy for developers to use it as a part of more sophisticated
;;    plugins. If it isn't, your input is welcome.

;; Developer Usage (informal API):
;; - `purpose-set-window-layout', `purpose-load-window-layout': use this
;;    to set a window layout that suits your plugin.  -
;; - `purpose-get-window-layout' or `purpose-save-window-layout': use
;;    this to save a layout so you can add it to your plugin later.
;; - Functions for changing frame layout (similar to window layout)
;; - `purpose-get-extra-window-params-function': use this if you want to
;;    save additional window parameters that make sense for your plugin,
;;    when `purpose-get-window-layout' is called.
;; - `purpose-set-window-properties-functions': use this hook if you
;;    want to set extra properties for new windows, when
;;    `purpose-set-window-layout' is called.
;; - `purpose-set-configuration', `purpose-add-configuration': use these
;;    to change the purpose configuration to suit your plugin's needs.
;; - `purpose-select-buffer-hook': use this if you want to run some
;;    code every time a buffer is selected.
;; - `without-purpose': use this macro if you need to ignore purposes
;;    while executing some piece of code.
;; - `without-purpose-command': use this macro to create a command that
;;    ignores purposes.
;; - `purpose-special-action-sequences': use this to display some
;;    buffers in a special way.

;;; Code:

(require 'purpose-configuration)
(require 'purpose-core)
(require 'purpose-layout)
(require 'purpose-switch)
(require 'purpose-prefix-overload)

(defconst purpose-version "1.1.50"
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
  (if (fboundp 'advice-add)
      ;; add advices for Emacs 24.4 and newer
      (progn 
	(advice-add 'switch-to-buffer
		    :around #'purpose-switch-to-buffer-advice)
	(advice-add 'switch-to-buffer-other-window
		    :around #'purpose-switch-to-buffer-other-window-advice)
	(advice-add 'switch-to-buffer-other-frame
		    :around #'purpose-switch-to-buffer-other-frame-advice)
	(advice-add 'pop-to-buffer :around #'purpose-pop-to-buffer-advice)
	(advice-add 'pop-to-buffer-same-window
		    :around #'purpose-pop-to-buffer-same-window-advice)
	(advice-add 'display-buffer :around #'purpose-display-buffer-advice))
    ;; add advices for Emacs 24.3 and older
    (dolist (function '(switch-to-buffer
			switch-to-buffer-other-window
			switch-to-buffer-other-frame
			pop-to-buffer
			pop-to-buffer-same-window))
      (ad-enable-advice function 'around 'purpose-override)
      (ad-update function)
      (ad-activate function))))

(defun purpose--remove-advices ()
  "Remove all advices needed for Purpose to work.
This function is called when `purpose-mode' is deactivated."
  (if (fboundp 'advice-remove)
      ;; remove advices for Emacs 24.4 and newer
      (progn
	(advice-remove 'switch-to-buffer #'purpose-switch-to-buffer-advice)
	(advice-remove 'switch-to-buffer-other-window
		       #'purpose-switch-to-buffer-other-window-advice)
	(advice-remove 'switch-to-buffer-other-frame
		       #'purpose-switch-to-buffer-other-frame-advice)
	(advice-remove 'pop-to-buffer #'purpose-pop-to-buffer-advice)
	(advice-remove 'pop-to-buffer-same-window
		       #'purpose-pop-to-buffer-same-window-advice)
	(advice-remove 'display-buffer #'purpose-display-buffer-advice))
    ;; remove advices for Emacs 24.3 and older
    (dolist (function '(switch-to-buffer
			switch-to-buffer-other-window
			switch-to-buffer-other-frame
			pop-to-buffer
			pop-to-buffer-same-window))
      (ad-disable-advice function 'around 'purpose-override)
      (ad-update function))))

(define-minor-mode purpose-mode
  nil :global t :lighter (:eval (purpose--modeline-string))
  (if purpose-mode
      (progn
	(purpose--add-advices)
	(setq display-buffer-overriding-action
	      '(purpose--action-function . nil))
	(setq purpose--active-p t))
    (purpose--remove-advices)
    (setq purpose--active-p nil)))

(provide 'purpose)
;;; purpose.el ends here
