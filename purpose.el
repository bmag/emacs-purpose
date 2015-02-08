;;; purpose.el --- Handle buffers and windows by their purposes

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 1.0.50

;;; Commentary:

;; Purpose is a package that introduces the concept of a "purpose" for
;; windows and buffers, and then helps you maintain a robust window
;; layout easily. Purpose is intended to help both regular users and
;; developers who want Emacs to have a more IDE-like behavior.

;; Typical Usage (Regular User)
;; 1. Turn Purpose on (`purpose-mode').
;; 2. Configure which purposes you want your windows to have (see
;;    pu-configuration.el).
;; 3. Arrange your window layout as you want it to be. Any window which
;;    you want to dedicate to a specific purpose (so it won't be used
;;    for other purposes), you shuld dedicate with
;;    `pu:toggle-window-purpose-dedicated'.
;; 4. Use purpose-aware commands instead of your regular commands when
;;    you need to change buffers (e.g. `pu:switch-buffer' instead of
;;    `switch-to-buffer'). This will open your buffers in the correct
;;    windows.
;; -  To save your layout, or load a previously saved layout, use
;;    `pu:save-layout' and `pu:load-layout'. You can load a saved layout
;;    and skip phases 1 and 2, of course.

;; Purpose-Aware commands that replace common commands:
;; `pu:switch-buffer': instead of `switch-to-buffer'
;; `pu:pop-buffer': instead of `pop-to-buffer'
;; `pu:find-file': instead of `find-file'
;; `pu:find-file-other-window': instead of `find-file-other-window'

;; Important Features:
;; -  Configurable: Configure how Purpose decides what's your buffer's
;;    purpose. Note that the window's purpose is determined by its
;;    buffer.
;; -  Persistent Window Layout: You can save and load your window layout
;;    between sessions by using `pu:save-layout' and `pu:load-layout'.
;; -  Purpose-Aware Buffer Switching: Commands for switching buffers
;;    without ruining your layout. The main ones are `pu:switch-buffer',
;;    `pu:pop-buffer' and `pu:find-file'. Also, purpose-aware switching
;;    is supported for any function that uses `display-buffer'
;;    internally (`switch-to-buffer' doesn't). See pu-switch.el for
;;    more.
;; -  Developer-Friendly: Purpose has hooks and an API that should make
;;    it easy for developers to use it as a part of more sophisticated
;;    plugins. If it isn't, your input is welcome.

;; Developer Usage (informal API):
;; -  `pu:set-layout', `pu:load-layout': use this to set a window layout
;;    that suits your plugin.
;; -  `pu:get-layout' or `pu:save-layout': use this to save a layout so
;;    you can add it to your plugin later.
;; -  `pu:get-extra-window-params-function': use this if you want to
;;    save additional window parameters that make sense for your plugin,
;;    when `pu:get-layout' is called.
;; -  `pu:set-window-properties-functions': use this hook if you want to
;;    set extra properties for new windows, when `pu:set-layout' is
;;    called.
;; -  `set-configuration', `add-configuration': use these to change the
;;    purpose configuration to suit your plugin's needs.
;; -  `with-action-function-inactive': use this macro if you need
;;    `display-buffer' to ignore purposes (original behavior) while
;;    executing some piece of code.
;; -  `pu:display-buffer-hook': use this if you want to run some code
;;    every time a buffer is displayed.

;;; Installation:
;; Download Purpose's source files and put them in your `load-path'.
;; -  Note: Purpose is not yet on any package repository. Once it will
;;    be there, you could download it with Emacs' package manager. For
;;    now, you have to do it manually.
;; Add these lines to your init file:
;;    (require 'purpose)
;;    (purpose-mode)

;;; Code:

(require 'pu-core)
(require 'pu-configuration)
(require 'pu-layout)
(require 'pu-switch)
(require 'pu-prefix-overload)

(pu:def-prefix-overload pu:find-file-overload '(pu:find-file ido-find-file))
(pu:def-prefix-overload pu:find-file-other-window-overload '(pu:find-file-other-window ido-find-file-other-window))
(pu:def-prefix-overload pu:switch-buffer-overload '(pu:switch-buffer ido-switch-buffer))
(pu:def-prefix-overload pu:pop-buffer-overload '(pu:pop-buffer ido-switch-buffer-other-window))


(defvar purpose-mode-map
  (let ((map (make-sparse-keymap)))
    ;; No "C-x 5" bindings because we don't support multiple frames yet
    (define-key map (kbd "C-x C-f") #'pu:find-file-overload)
    (define-key map (kbd "C-x 4 f") #'pu:find-file-other-window-overload)
    (define-key map (kbd "C-x 4 C-f") #'pu:find-file-other-window-overload)
    (define-key map (kbd "C-x b") #'pu:switch-buffer-overload)
    (define-key map (kbd "C-x 4 b") #'pu:pop-buffer-overload)

    ;; Helpful for quitting temporary windows. Close in meaning to
    ;; `kill-buffer', so we map it to a close key ("C-x j" is close to
    ;; "C-x k")
    (define-key map (kbd "C-x j") #'quit-window)

    ;; We use "C-c ," for compatibility with key-binding conventions
    (define-key map (kbd "C-c ,") 'purpose-mode-prefix-map)
    (define-prefix-command 'purpose-mode-prefix-map)
    (define-key purpose-mode-prefix-map (kbd "o") #'pu:switch-buffer)
    (define-key purpose-mode-prefix-map (kbd "p") #'pu:pop-buffer)
    (define-key purpose-mode-prefix-map (kbd "d") #'pu:toggle-window-purpose-dedicated)
    (define-key purpose-mode-prefix-map (kbd "D") #'pu:toggle-window-buffer-dedicated)

    map)
  "Keymap for Purpose mode.")

(defun pu:modeline-string ()
  "Return the presentation of a window's purpose for display in the
modeline. The string returned has two forms. For example, if window's
purpose is 'edit:
If (pu:window-purpose-dedicated-p), return \"[edit!]\", otherwise return
\"[edit]\"."
  (format "[%s%s]"
	  (pu:window-purpose)
	  (if (pu:window-purpose-dedicated-p) "!" "")))

(define-minor-mode purpose-mode
  nil :global t :lighter (:eval (pu:modeline-string))
  (if purpose-mode
      (progn
	(setq display-buffer-overriding-action '(pu:action-function . nil))
	(setq pu:action-function-active-p t))
    (setq pu:action-function-active-p nil)))

(provide 'purpose)
;;; purpose.el ends here
