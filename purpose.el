;;; purpose.el

;; Author: Bar Magal (2015)
;; Package: purpose
;; Version: 0.1.0

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

;; Important Features:
;; -  Configurable: Configure how Purpose decides what's your buffer's
;;    purpose. Note that the window's purpose is determined by its buffer.
;; -  Persistent Window Layout: You can save and load your window layout
;;    between sessions by using `pu:save-layout' and `pu:load-layout'.
;; -  Purpose-Aware Buffer Switching: Commands for switching buffers
;;    without ruining your layout. The main ones are `pu:switch-buffer'
;;    and `pu:pop-buffer'. Also, purpose-aware switching is supported
;;    for any function that uses `display-buffer' internally
;;    (`switch-to-buffer'  doesn't). See pu-switch.el for more.
;; -  Developer-Friendly: Purpose has hooks and an API that should make
;;    it easy for developers to use it as a part of more sophisticated
;;    plugins. If it isn't, your input is welcome.

;; Developer Usage (informal API):
;; -  `pu:set-layout', `pu:load-layout': use this to set a window layout that
;;    suits your plugin.
;; -  `pu:get-layout' or `pu:save-layout': use this to save a layout so you
;;    can add it to your plugin later.
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

(defvar purpose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") #'pu:switch-buffer)
    (define-key map (kbd "C-c p") #'pu:pop-buffer)
    (define-key map (kbd "C-x j") #'quit-window)
    (define-key map (kbd "C-c d") #'pu:toggle-window-purpose-dedicated)
    (define-key map (kbd "C-c D") #'pu:toggle-window-buffer-dedicated)
    map)
  "Keymap for Purpose mode.")

(define-minor-mode purpose-mode
  nil :global t
  (if purpose-mode
      (progn
	(setq display-buffer-overriding-action '(pu:action-function . nil))
	(setq pu:action-function-active-p t))
    (setq pu:action-function-active-p nil)))

(provide 'purpose)
