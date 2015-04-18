;;; window-purpose-test.el --- Tests for Purpose -*- lexical-binding: t -*-

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
;; This file contains tests for Purpose.

;;; Code:

(message "defining tests")

(defvar test-happened nil
  "Variable for use in tests.
Set the value of this variable at the beginning of each test that uses
it.")

(unless (fboundp 'hash-table-keys)
  (defun hash-table-keys (hash-table)
    "Return a list of keys in HASH-TABLE."
    (let (result)
      (maphash #'(lambda (key value)
		   (setq result (append (list key) result)))
	       hash-table)
      result))

  (defun hash-table-values (hash-table)
    "Return a list of values in HASH-TABLE."
    (let (result)
      (maphash #'(lambda (key value)
		   (setq result (append (list value) result)))
	       hash-table)
      result)))



;;; purpose-configuration.el

(defmacro purpose-with-empty-config (&rest body)
  `(let ((purpose--user-mode-purposes (make-hash-table))
	 (purpose--user-name-purposes (make-hash-table :test #'equal))
	 (purpose--user-regexp-purposes (make-hash-table :test #'equal))
	 (purpose--extended-mode-purposes (make-hash-table))
	 (purpose--extended-name-purposes (make-hash-table :test #'equal))
	 (purpose--extended-regexp-purposes (make-hash-table :test #'equal))
	 (purpose--default-mode-purposes (make-hash-table))
	 (purpose--default-name-purposes (make-hash-table :test #'equal))
	 (purpose--default-regexp-purposes (make-hash-table :test #'equal))
	 (purpose-use-default-configuration t)
	 purpose-user-mode-purposes
	 purpose-user-name-purposes
	 purpose-user-regexp-purposes
	 purpose-extended-configuration)
     ,@body))

(defmacro purpose-with-temp-config (modes names regexps &rest body)
  `(purpose-with-empty-config
    (let ((purpose-user-mode-purposes ,modes)
	  (purpose-user-name-purposes ,names)
	  (purpose-user-regexp-purposes ,regexps))
      (purpose-compile-user-configuration)
      ,@body)))

(ert-deftest purpose-test-compile-user-config ()
  "Test compilation of user configuration.
This tests that `purpose-compile-user-configuration' creates the correct
hash tables for the uncompiled alists."
  (purpose-with-empty-config
   (let
       ((purpose-user-mode-purposes '((prog-mode . edit)
				      (dired-mode . dired)))
	(purpose-user-name-purposes '(("editor" . edit)
				      ("foo" . bar)))
	(purpose-user-regexp-purposes '(("^\\*" . common))))
     (purpose-compile-user-configuration)
     (should (equal (hash-table-keys purpose--user-mode-purposes) '(dired-mode prog-mode)))
     (should (equal (hash-table-values purpose--user-mode-purposes) '(dired edit)))
     (should (equal (hash-table-keys purpose--user-name-purposes) '("foo" "editor")))
     (should (equal (hash-table-values purpose--user-name-purposes) '(bar edit)))
     (should (equal (hash-table-keys purpose--user-regexp-purposes) '("^\\*")))
     (should (equal (hash-table-values purpose--user-regexp-purposes) '(common))))
   (let (purpose-user-mode-purposes
	 purpose-user-name-purposes
	 purpose-user-regexp-purposes)
     (purpose-compile-user-configuration)
     (should (equal (hash-table-count purpose--user-mode-purposes) 0))
     (should (equal (hash-table-count purpose--user-name-purposes) 0))
     (should (equal (hash-table-count purpose--user-regexp-purposes) 0)))))


(ert-deftest purpose-test-compile-ext-config ()
  "Test compilation of extended configuration.
This tests that `purpose-compile-extended-configuration' creates the
correct hash tables for the uncompiled alists."
  (purpose-with-empty-config
   (let
       ((purpose-extended-conf
	 (purpose-conf "test"
		       :mode-purposes '((prog-mode . edit)
					(dired-mode . dired))
		       :name-purposes '(("editor" . edit)
					("foo" . bar))
		       :regexp-purposes '(("^\\*" . common)))))
     (purpose-set-extension-configuration :test purpose-extended-conf)
     (should (equal (hash-table-keys purpose--extended-mode-purposes) '(dired-mode prog-mode)))
     (should (equal (hash-table-values purpose--extended-mode-purposes) '(dired edit)))
     (should (equal (hash-table-keys purpose--extended-name-purposes) '("foo" "editor")))
     (should (equal (hash-table-values purpose--extended-name-purposes) '(bar edit)))
     (should (equal (hash-table-keys purpose--extended-regexp-purposes) '("^\\*")))
     (should (equal (hash-table-values purpose--extended-regexp-purposes) '(common))))
   (let (purpose-extended-configuration)
     (purpose-compile-extended-configuration)
     (should (equal (hash-table-count purpose--extended-mode-purposes) 0))
     (should (equal (hash-table-count purpose--extended-name-purposes) 0))
     (should (equal (hash-table-count purpose--extended-regexp-purposes) 0)))))


(ert-deftest purpose-test-compile-default-config ()
  "Test compilation of default configuration.
This tests that `purpose-compile-default-configuration' creates the
correct hash tables."
  (purpose-with-empty-config
   (purpose-compile-default-configuration)
   (should (equal (hash-table-keys purpose--default-mode-purposes)
		  '(package-menu-mode
		    image-mode
		    compilation-mode
		    grep-mode
		    occur-mode
		    Buffer-menu-mode
		    ibuffer-mode
		    dired-mode
		    comint-mode
		    text-mode
		    prog-mode)))
   (should (equal (hash-table-values purpose--default-mode-purposes)
		  '(package
		    image
		    search
		    search
		    search
		    buffers
		    buffers
		    dired
		    terminal
		    edit
		    edit)))
   (should (equal (hash-table-keys purpose--default-name-purposes)
		  '("*shell*" ".hgignore" ".gitignore")))
   (should (equal (hash-table-values purpose--default-name-purposes)
		  '(terminal edit edit)))
   (should (equal (hash-table-keys purpose--default-regexp-purposes)
                  '("^ \\*Minibuf-[0-9]*\\*$")))
   (should (equal (hash-table-values purpose--default-regexp-purposes)
                  '(minibuf)))))

(ert-deftest purpose-test-set-ext-conf-error ()
  "Test error cases for settings/deleting extension configuration.
See `purpose-set-extension-configuration' and
`purpose-del-extension-configuration'."
  (purpose-with-empty-config
   (should-error (purpose-set-extension-configuration 'foo (purpose-conf "foo")))
   (purpose-set-extension-configuration :foo (purpose-conf "foo"))
   (should-error (purpose-del-extension-configuration 'foo))
   (purpose-del-extension-configuration :foo)))


;;; purpose-core.el

(ert-deftest purpose-test-dummy-buffer-name ()
  "Test generation of dummy buffer names"
  (should (equal (purpose--dummy-buffer-name 'edit) "*pu-dummy-edit*")))

(ert-deftest purpose-test-mode-purpose ()
  "Test `purpose--buffer-purpose-mode' returns correct values."
  (purpose-with-temp-config
   '((prog-mode . prog) (c-mode . c) (text-mode . text))
   nil nil
   (with-temp-buffer
     (let ((c++-mode-hook nil)
	   (c-mode-hook nil)
	   (text-mode-hook nil))
       (c++-mode)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'prog))
       (c-mode)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'c))
       (text-mode)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'text))))))

(ert-deftest purpose-test-name-purpose ()
  "Test `purpose--buffer-purpose-name' returns correct values."
  (purpose-with-temp-config
   nil
   '(("hello" . hello) ("foo" . foo))
   nil
   (with-temp-buffer
     (rename-buffer "hello" t)
     (should (equal (purpose-buffer-purpose (current-buffer)) 'hello))
     (rename-buffer "foo" t)
     (should (equal (purpose-buffer-purpose (current-buffer)) 'foo)))))

(ert-deftest purpose-test-regexp-purpose ()
  "Test `purpose--buffer-purpose-regexp' returns correct values."
  (purpose-with-temp-config
   nil nil
   '(("^hello$" . hello) ("^\\*foo" . foo))
   (with-temp-buffer
     (rename-buffer "hello" t)
     (should (equal (purpose-buffer-purpose (current-buffer)) 'hello))
     (rename-buffer "*foo bar*" t)
     (should (equal (purpose-buffer-purpose (current-buffer)) 'foo)))))

(ert-deftest purpose-test-buffer-purpose ()
  "Test `purpose-buffer-purpose' returns correct values."
  (purpose-with-temp-config
   '((c-mode . c))
   '(("foo" . foo-by-name) ("*foo bar*" . foo-bar))
   '(("^\\*foo" . foo-by-regexp))
   (with-temp-buffer
     (let ((c-mode-hook nil)
	   (default-purpose 'some-default))
       (should (equal (purpose-buffer-purpose (current-buffer)) default-purpose))
       (c-mode)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'c))
       ;; regexp overrides mode
       (rename-buffer "*foo*" t)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'foo-by-regexp))
       ;; name overrides regexp and mode
       (rename-buffer "*foo bar*" t)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'foo-bar))
       ;; dummy buffer overrides anything else
       (rename-buffer "*pu-dummy-xxx*" t)
       (should (equal (purpose-buffer-purpose (current-buffer)) 'xxx))))))

(defun purpose-kill-buffers-safely (&rest buffers)
  "Safely kill BUFFERS.
Each item in BUFFERS is either a buffer or a buffer's name."
  (let ((kill-buffer-query-functions nil)
	(kill-buffer-hook nil))
    (mapc #'(lambda (buf) (ignore-errors (kill-buffer buf))) buffers)))

(ert-deftest purpose-test-buffers-with-purpose ()
  "Test `purpose-buffers-with-purpose'."
  (purpose-with-temp-config
   nil nil '(("xxx-test" . test))
   (unwind-protect
       (let (buffers)
         (push (get-buffer-create "xxx-test-1") buffers)
         (push (get-buffer-create "xxx-test-2") buffers)
         (get-buffer-create "another-buffer")
         (should (or (equal (purpose-buffers-with-purpose 'test) buffers)
                     (equal (purpose-buffers-with-purpose 'test) (reverse buffers)))))
     (purpose-kill-buffers-safely "xxx-test-1" "xxx-test-2" "another-buffer"))))

(ert-deftest purpose-test-window-purpose ()
  "Test `purpose-window-purpose'."
  (unwind-protect
      (save-window-excursion
	(set-window-buffer nil (get-buffer-create "xxx-test-1"))
	(purpose-with-temp-config
	 nil '(("xxx-test-1" . test)) nil
	 (should (equal (purpose-window-purpose) 'test))))
    (purpose-kill-buffers-safely "xxx-test-1")))

(ert-deftest purpose-test-windows-with-purpose ()
  "Test `purpose-windows-with-purpose'."
  (unwind-protect
      (save-window-excursion
	(let (windows
	      (split-width-threshold 1)
	      (split-height-threshold 1))
	  (delete-other-windows)
	  (set-window-buffer nil (get-buffer-create "xxx-test-1"))
    (push (selected-window) windows)
	  (select-window (split-window))
	  (set-window-buffer nil (get-buffer-create "another-buffer"))
	  (purpose-with-temp-config
	   nil '(("another-buffer" . foo)) '(("xxx-test" . test))
	   (should (equal (purpose-windows-with-purpose 'test) windows)))))
    (purpose-kill-buffers-safely "xxx-test-1" "another-buffer")))

(ert-deftest purpose-test-get-buffer-create ()
  "Test `purpose--get-buffer-create' returns/creates correct buffer."
  (unwind-protect
      (purpose-with-temp-config
       nil '(("xxx-test" . test)) nil
       (should (equal (buffer-name (purpose--get-buffer-create 'test)) "*pu-dummy-test*"))
       (purpose-kill-buffers-safely "*pu-dummy-test*")
       (get-buffer-create "xxx-test")
       (should (equal (buffer-name (purpose--get-buffer-create 'test)) "xxx-test")))
    (purpose-kill-buffers-safely "*pu-dummy-test*" "xxx-test")))

(ert-deftest purpose-test-set-window-buffer ()
  "Test `purpose--set-window-buffer' sets correct buffer and window."
  (unwind-protect
      (save-window-excursion
	(purpose-with-empty-config
	 (delete-other-windows)
	 (let* ((window (selected-window))
		(other-window (split-window window)))
	   (purpose--set-window-buffer 'test)
	   (should (equal (buffer-name (window-buffer window)) "*pu-dummy-test*"))
	   (should-not (equal (purpose-window-purpose other-window) 'test)))))
    (purpose-kill-buffers-safely "*pu-dummy-test*")))

(ert-deftest purpose-test-dedication-toggle ()
  "Test toggling of window dedication (purpose and buffer)."
  (let ((buffer-dedication (window-dedicated-p))
	(purpose-dedication (purpose-window-purpose-dedicated-p)))
    (unwind-protect
	(progn
	  ;; buffer dedication
	  (set-window-dedicated-p nil nil)
	  (purpose-toggle-window-buffer-dedicated)
	  (should (window-dedicated-p))
	  (purpose-toggle-window-buffer-dedicated)
	  (should-not (window-dedicated-p))
	  ;; purpose dedication
	  (purpose-set-window-purpose-dedicated-p nil nil)
	  (purpose-toggle-window-purpose-dedicated)
	  (should (purpose-window-purpose-dedicated-p))
	  (purpose-toggle-window-purpose-dedicated)
	  (should-not (purpose-window-purpose-dedicated-p)))
      (set-window-dedicated-p nil buffer-dedication)
      (purpose-set-window-purpose-dedicated-p nil purpose-dedication))))

(ert-deftest purpose-test-get-window ()
  "Test functions for getting top/bottom/left/right windows.
Functions tested are:
- `purpose-get-top-window'
- `purpose-get-bottom-window'
- `purpose-get-left-window'
- `purpose-get-right-window'"
  (save-window-excursion
    (delete-other-windows)
    (should-not (purpose-get-top-window))
    (should-not (purpose-get-bottom-window))
    (should-not (purpose-get-left-window))
    (should-not (purpose-get-right-window))
    (let ((top-window (selected-window))
	  (bottom-window (split-window nil 5 'below)))
      (should (equal (purpose-get-top-window) top-window))
      (should (equal (purpose-get-bottom-window) bottom-window))
      (should-not (purpose-get-left-window))
      (should-not (purpose-get-right-window)))
    (delete-other-windows)
    (let ((left-window (selected-window))
	  (right-window (split-window nil 5 'right)))
      (should (equal (purpose-get-left-window) left-window))
      (should (equal (purpose-get-right-window) right-window))
      (should-not (purpose-get-top-window))
      (should-not (purpose-get-bottom-window)))))



;;; purpose-prefix-overload.el

(defmacro purpose-call-with-prefix-arg (arg command)
  `(let ((current-prefix-arg ,arg))
     (call-interactively ,command)))

(ert-deftest purpose-test-prefix-overload ()
  "Test `define-purpose-prefix-overload' works correctly."
  (define-purpose-prefix-overload --purpose-prefix-test
    '((lambda () (interactive) 0)
      (lambda () (interactive) 1)
      (lambda () (interactive) 2)
      (lambda () (interactive) 3)))
  (should (equal 0 (purpose-call-with-prefix-arg nil '--purpose-prefix-test)))
  (should (equal 0 (purpose-call-with-prefix-arg 0 '--purpose-prefix-test)))
  (should (equal 1 (purpose-call-with-prefix-arg 1 '--purpose-prefix-test)))
  (should (equal 2 (purpose-call-with-prefix-arg 2 '--purpose-prefix-test)))
  (should (equal 3 (purpose-call-with-prefix-arg 3 '--purpose-prefix-test)))
  (should-error (purpose-call-with-prefix-arg 4 '--purpose-prefix-test))
  (should (equal 1 (purpose-call-with-prefix-arg '(4) '--purpose-prefix-test)))
  (should (equal 2 (purpose-call-with-prefix-arg '(16) '--purpose-prefix-test)))
  (should (equal 3 (purpose-call-with-prefix-arg '(64) '--purpose-prefix-test)))
  (should-error (purpose-call-with-prefix-arg '(256) '--purpose-prefix-test)))




;;; purpose-switch.el

(defun purpose-create-buffers (&rest buffer-names)
  "Create buffers according to BUFFER-NAMES."
  (mapcar #'get-buffer-create buffer-names))

(cl-defun purpose-create-buffers-for-test (&key (p0 0) &key (p1 0) &key (p2 0))
  "Create buffers for purposes 'p0, 'p1 and 'p2.
P0, P1 and P2 should be integers denoting how many buffers should be
created for each purpose.
The buffers created have the names \"xxx-p0-0\", \"xxx-p0-1\",
\"xxx-p1-0\", \"xxx-p1-1\", \"xxx-p2-0\", etc."
  (cl-loop for times in (list p0 p1 p2)
	   for purpose in '("p0" "p1" "p2")
	   do (dotimes (index times)
		(purpose-create-buffers (format "xxx-%s-%s" purpose index)))))

(defun purpose-displayed-buffers (&optional frame)
  "Return a list of buffers displayed in FRAME."
  (mapcar #'window-buffer (window-list frame)))

(defun purpose-displayed-buffer-names (&optional frame)
  (mapcar #'buffer-name (purpose-displayed-buffers frame)))

(defmacro purpose-check-displayed-buffers (buffer-names)
  `(should (equal (sort (purpose-displayed-buffer-names) #'string-lessp)
		  (sort ,buffer-names #'string-lessp))))

(ert-deftest purpose-test-switch-buffer ()
  "Test variations of `purpose-switch-buffer'.
- 1 windows, switch to same purpose
- 1 window, switch to different purpose
- 1 window, p-dedicated, switch to same purpose
- 1 window, p-dedicated, switch to different purpose
- 1 window, b-dedicated, switch to same purpose
- 1 window, b-dedicated, switch to different purpose
- 2 windows (purposes p0 and p1), from p1 window, switch to buried p0
  buffer"
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 2 :p1 1)
	   (purpose-mode 1)
	   ;; 1
	   (message "1...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-switch-buffer "xxx-p0-1")
	   (switch-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1"))
	   ;; 2
	   (message "2...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-switch-buffer "xxx-p1-0")
	   ;; (switch-to-buffer "xxx-p1-0")
	   (purpose-check-displayed-buffers '("xxx-p1-0"))
	   ;; 3
	   (message "3...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-set-window-purpose-dedicated-p nil t)
	   ;; (purpose-switch-buffer "xxx-p0-1")
	   (switch-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1"))
	   (purpose-set-window-purpose-dedicated-p nil nil)
	   ;; 4
	   (message "4...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-set-window-purpose-dedicated-p nil t)
	   ;; (purpose-switch-buffer "xxx-p1-0")
	   (switch-to-buffer "xxx-p1-0")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-0"))
	   (purpose-set-window-purpose-dedicated-p nil nil)
	   ;; 5
	   (message "5...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-1")
	   (set-window-dedicated-p nil t)
	   ;; (purpose-switch-buffer "xxx-p0-1")
	   (switch-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1"))
	   (set-window-dedicated-p nil nil)
	   ;; 6
	   (message "6...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   ;; (purpose-switch-buffer "xxx-p1-0")
	   (switch-to-buffer "xxx-p1-0")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-0"))
	   (set-window-dedicated-p nil nil)
	   ;; 7
	   (message "7...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p1-0")
	   ;; (purpose-switch-buffer "xxx-p0-1")
	   (switch-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1" "xxx-p1-0"))
	   (purpose-mode -1)))
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))))

(ert-deftest purpose-test-switch-buffer-other-window ()
  "Test variations of `purpose-switch-buffer-other-window'.
- 1 windows, switch to same purpose
- 2 windows (p0 and p1), from p0 window, switch to buried p0 buffer
- 2 windows (p0 and p1), p1 dedicated, from p0 window, switch to buried
  p0 buffer."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 2 :p1 1)
	   (purpose-mode 1)
	   ;; 1
	   (message "1...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-switch-buffer-other-window "xxx-p0-1")
	   (switch-to-buffer-other-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1"))
	   ;; 2
	   (message "2...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-switch-buffer-other-window "xxx-p0-1")
	   ;; (switch-to-buffer-other-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1"))
	   ;; 3
	   (message "3...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (purpose-set-window-purpose-dedicated-p nil t)
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-switch-buffer-other-window "xxx-p0-1")
	   (switch-to-buffer-other-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))
	   (purpose-mode -1)))
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))))

(ert-deftest purpose-test-pop-buffer ()
  "Test variations of `purpose-pop-buffer'.
- 1 windows, switch to same purpose
- 2 windows (p0 and p1), from p0 window, switch to buried p0 buffer
- 2 windows (p0 and p1), p1 dedicated, from p0 window, switch to buried
  p0 buffer."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 2 :p1 1)
	   (purpose-mode 1)
	   ;; 1
	   (message "1...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-pop-buffer "xxx-p0-1")
	   (pop-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1"))
	   ;; 2
	   (message "2...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-pop-buffer "xxx-p0-1")
	   ;; (pop-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1" "xxx-p1-0"))
	   ;; 3
	   (message "3...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (purpose-set-window-purpose-dedicated-p nil t)
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-pop-buffer "xxx-p0-1")
	   (pop-to-buffer "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1" "xxx-p1-0"))
	   (purpose-mode -1)))
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))))

(ert-deftest purpose-test-pop-buffer-same-window ()
  "Test variations of `purpose-pop-buffer-same-window'.
- 1 windows, switch to other purpose
- 2 windows (p0 and p1), from p0 window, switch to buried p0 buffer
- 2 windows (p0 and p1), from p1 window, switch to buried p0 buffer
- 2 windows (p0 and p1), p0 b-dedicated, from p0 window, switch to
  buried p0 buffer.
- 2 windows (p0 and p1), both b-dedicated, from p0 window, switch to
  buried p0 buffer."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 2 :p1 1)
	   (purpose-mode 1)
	   ;; 1
	   (message "1...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   ;; (purpose-pop-buffer-same-window "xxx-p1-0")
	   (pop-to-buffer-same-window "xxx-p1-0")
	   (purpose-check-displayed-buffers '("xxx-p1-0"))
	   ;; 2
	   (message "2...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-pop-buffer-same-window "xxx-p0-1")
	   ;; (pop-to-buffer-same-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-1" "xxx-p1-0"))
	   ;; 3
	   (message "3...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p1-0")
	   ;; (purpose-pop-buffer-same-window "xxx-p0-1")
	   (pop-to-buffer-same-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1"))
	   ;; 4
	   (message "4...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   ;; (purpose-pop-buffer-same-window "xxx-p0-1")
	   (pop-to-buffer-same-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1"))
	   ;; 5
	   (message "5...")
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p1-0")
	   (set-window-dedicated-p nil t)
	   (select-window (split-window))
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   ;; (purpose-pop-buffer-same-window "xxx-p0-1")
	   (pop-to-buffer-same-window "xxx-p0-1")
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))
	   (purpose-mode -1)))
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))))

;; can't raise frames in automatic tests (because "emacs -batch"), so
;; this test can't pass...
;; (ert-deftest purpose-test-switch-buffer-other-frame ()
;;   "Test `purpose-switch-buffer-other-frame'."
;;   (let ((frame-conf (current-frame-configuration)))
;;     (unwind-protect
;; 	(unwind-protect
;; 	    (purpose-with-temp-config
;; 	     nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
;; 	     (purpose-create-buffers-for-test :p0 2 :p1 1)
;; 	     (purpose-mode 1)
;; 	     (delete-other-frames)
;; 	     (delete-other-windows)
;; 	     (set-window-buffer nil "xxx-p0-0")
;; 	     ;; (purpose-switch-buffer-other-frame "xxx-p0-1")
;;           (switch-to-buffer-other-frame "xxx-p0-1")
;; 	     (should (equal (length (frame-list)) 2))
;; 	     (purpose-check-displayed-buffers "xxx-p0-1")
;; 	     (other-frame 1)
;; 	     (purpose-check-displayed-buffers "xxx-p0-0")
;; 	     (purpose-mode -1))
;; 	  (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))
;;       (set-frame-configuration frame-conf))))

(ert-deftest purpose-test-window-buffer-reusable ()
  "Test `purpose-window-buffer-reusable-p'."
  (should (purpose-window-buffer-reusable-p (selected-window) (window-buffer (selected-window)))))

(ert-deftest purpose-test-display-fallback-pop-window ()
  "Test value `pop-up-window' for `purpose-display-fallback'.
From single buffer-dedicated window, switch with `force-same-window: a
new window should be created."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 1 :p1 1)
	   (purpose-mode 1)
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   (let ((purpose-display-fallback 'pop-up-window))
	     (switch-to-buffer "xxx-p1-0" nil t)
	     (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-0")))))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0"))))

;; can't raise frames in automatic tests (because "emacs -batch"), so
;; this test can't pass...
;; (ert-deftest purpose-test-display-fallback-pop-frame ()
;;   "Test value `pop-up-frame' for `purpose-display-fallback'.
;; From single buffer-dedicated window, switch with `force-same-window: a
;; new frame should be created."
;;   (unwind-protect
;;       (let ((purpose-message-on-p t))
;; 	(purpose-with-temp-config
;; 	 nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
;; 	 (purpose-create-buffers-for-test :p0 1 :p1 1)
;; 	 (purpose-mode 1)
;; 	 (delete-other-frames)
;; 	 (delete-other-windows)
;; 	 (set-window-buffer nil "xxx-p0-0")
;; 	 (set-window-dedicated-p nil t)
;; 	 (let ((purpose-display-fallback 'pop-up-frame))
;; 	   (switch-to-buffer "xxx-p1-0" nil t)
;; 	   (should (equal (length (frame-list)) 2)))))
;;     (purpose-mode -1)
;;     (delete-other-frames)
;;     (delete-other-windows)
;;     (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0")))

(ert-deftest purpose-test-display-fallback-error ()
  "Test value `error' for `purpose-display-fallback'.
From single buffer-dedicated window, switch with `force-same-window: an
error should be signaled."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1))
	   (purpose-create-buffers-for-test :p0 1 :p1 1)
	   (purpose-mode 1)
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   (let ((purpose-display-fallback 'error))
	     (should-error (switch-to-buffer "xxx-p1-0" nil t)))))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0"))))

(ert-deftest purpose-test-display-fallback-nil ()
  "Test value `nil' for `purpose-display-fallback'.
From single buffer-dedicated window, switch with `force-same-window': an
error should be signaled.
With two purpose-dedicated windows, switch to a third purpose window:
new buffer should be displayed in one of the two existing windows."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-message-on-p t))
	  (purpose-with-temp-config
	   nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1) ("^xxx-p2-" . p2))
	   (purpose-create-buffers-for-test :p0 1 :p1 1 :p2 1)
	   (purpose-mode 1)
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (set-window-dedicated-p nil t)
	   (let ((purpose-display-fallback nil))
	     (message "testing error ...")
	     (should-error (switch-to-buffer "xxx-p1-0" nil t)))

	   (set-window-dedicated-p nil nil)
	   (delete-other-windows)
	   (set-window-buffer nil "xxx-p0-0")
	   (purpose-set-window-purpose-dedicated-p nil t)
	   (let ((other-window (split-window)))
	     (set-window-buffer other-window "xxx-p1-0")
	     (purpose-set-window-purpose-dedicated-p other-window t))
	   (let ((purpose-display-fallback nil))
	     (message "testing successful display ...")
	     (switch-to-buffer "xxx-p2-0")
	     (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p2-0")))))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0"))))

(ert-deftest purpose-test-normalize-sizes ()
  "Test sanity for `purpose--normalize-width' and `purpose--normalize-height'."
  (message "testing purpose--normalize-width ...")
  (should (equal (purpose--normalize-width 5) 5))
  (should (equal (purpose--normalize-width 0.5) (/ (frame-width) 2)))
  (should (equal (purpose--normalize-width nil) nil))
  (should-error (purpose--normalize-width -5))

  (message "testing purpose--normalize-height ...")
  (should (equal (purpose--normalize-height 5) 5))
  (should (equal (purpose--normalize-height 0.5) (/ (frame-height) 2)))
  (should (equal (purpose--normalize-height nil) nil))
  (should-error (purpose--normalize-height -5)))

(ert-deftest purpose-test-display-at ()
  "Test scenarios of `purpose-display--at'."
  (save-window-excursion
    (unwind-protect
	(purpose-with-temp-config
	 nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1) ("^xxx-p2-" . p2))
	 (purpose-create-buffers-for-test :p0 1 :p1 2)
	 (purpose-mode 1)
	 (delete-other-windows)
	 (let ((first-window (selected-window)))
	   (set-window-buffer first-window "xxx-p0-0")
	   (message "testing popup ...")
	   (purpose-display-at-bottom (get-buffer "xxx-p1-0") nil)
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-0"))

	   (message "testing reuse buffer ...")
	   (select-window first-window)
	   (purpose-display-at-bottom (get-buffer "xxx-p1-0") nil)
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-0"))

	   (message "testing reuse purpose ...")
	   (select-window first-window)
	   (purpose-display-at-bottom (get-buffer "xxx-p1-1") nil)
	   (purpose-check-displayed-buffers '("xxx-p0-0" "xxx-p1-1"))))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0" "xxx-p1-1"))))

(ert-deftest purpose-test-special-action-sequences ()
  "Test `purpose-special-action-sequences' properly affects display behavior.
- (purpose . (display-functions)) causes <display-functions> to be
  called for buffers with <purpose>.
- (predicate . (display-functions)) causes <display-functions> to be
  called for buffers that match <predicate>."
  (save-window-excursion
    (unwind-protect
        (purpose-with-temp-config
         nil nil '(("^xxx-p0-" . p0) ("^xxx-p1-" . p1) ("^xxx-p2-" . p2))
         (purpose-create-buffers-for-test :p0 1 :p1 1 :p2 1)
         (purpose-mode 1)
         (setq test-happend 0)
         (let ((purpose-special-action-sequences
                '((p1
                   (lambda (buffer alist) (setq test-happened 1) (display-buffer-at-bottom buffer alist)))
                  ((lambda (purpose buffer alist) (eql (purpose-buffer-purpose buffer) 'p2))
                   (lambda (buffer alist) (setq test-happened 2) (display-buffer-at-bottom buffer alist))))))
           (delete-other-windows)
           (set-window-buffer nil "xxx-p0-0")
           (switch-to-buffer "xxx-p1-0")
           (message "Windows: %S" (window-list))
           (should (equal test-happened 1))
           (delete-other-windows)
           (set-window-buffer nil "xxx-p0-0")
           (switch-to-buffer "xxx-p2-0")
           (should (equal test-happened 2))))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-p0-0" "xxx-p1-0" "xxx-p2-0"))))

(ert-deftest purpose-cover-select-buffer-without-action-order ()
  "Test `purpose-select-buffer' does use `purpose-default-action-order'."
  (save-window-excursion
    (unwind-protect
	(let ((purpose-default-action-order 'prefer-other-window))
	  (purpose-mode 1)
	  (delete-other-windows)
	  (purpose-select-buffer (get-buffer-create "xxx-test"))
	  (should (equal (length (window-list)) 2)))
      (purpose-mode -1)
      (purpose-kill-buffers-safely "xxx-test"))))

;;; TODO:
;;; - alist entry `reuasble-frames'
;;; - `purpose-display-reuse-window-buffer-other-frame'
;;; - `purpose-display-reuse-window-purpose-other-frame'
;;; - `purpose-display-maybe-pop-up-frame'
;;; - `purpose-read-buffers-with-purpose'
;;; - `purpose-switch-buffer-with-purpose' (other-window, other-frame)


;;; purpose.el

(ert-deftest purpose-test-mode-line ()
  "Test `purpose--modeline-string' returns correct string."
  (save-window-excursion
    (unwind-protect
	(purpose-with-temp-config
	 nil '(("xxx-test" . test)) nil
	 (set-window-buffer nil (get-buffer-create "xxx-test"))
	 (purpose-set-window-purpose-dedicated-p nil t)
	 (should (equal (purpose--modeline-string) " [test!]"))
	 (purpose-set-window-purpose-dedicated-p nil nil)
	 (should (equal (purpose--modeline-string) " [test]")))
      (purpose-kill-buffers-safely "xxx-test")
      (purpose-set-window-purpose-dedicated-p nil nil))))



;;; purpose-layout.el

(ert-deftest purpose-test-delete-non-dedicated-windows ()
  "Test `purpose-delete-non-dedicated-windows'."
  (save-window-excursion
    (delete-other-windows)
    (let ((window (selected-window)))
      (purpose-set-window-purpose-dedicated-p window t)
      (split-window window)
      (purpose-delete-non-dedicated-windows)
      (should (equal (window-list) (list window))))))

(defun purpose-test-delete-window-at (display-fn delete-fn)
  (save-window-excursion
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (let ((window (selected-window)))
	    (should (funcall display-fn (get-buffer-create "xxx-test") nil))
	    (funcall delete-fn)
	    (should (equal (window-list) (list window)))
	    (should-error (funcall delete-fn))))
      (purpose-kill-buffers-safely "xxx-test"))))

(ert-deftest purpose-test-delete-window-at-top ()
  "Test `purpose-delete-window-at-top'."
  (purpose-test-delete-window-at #'purpose-display-at-top #'purpose-delete-window-at-top))

(ert-deftest purpose-test-delete-window-at-bottom ()
  "Test `purpose-delete-window-at-bottom'."
  (purpose-test-delete-window-at #'purpose-display-at-bottom #'purpose-delete-window-at-bottom))

(ert-deftest purpose-test-delete-window-at-left ()
  "Test `purpose-delete-window-at-left'."
  ;; split-window in batch mode doesn't work for big values of
  ;; `purpose-display-at-left-width'
  (let ((purpose-display-at-left-width 5))
    (purpose-test-delete-window-at #'purpose-display-at-left #'purpose-delete-window-at-left)))

(ert-deftest purpose-test-delete-window-at-right ()
  "Test `purpose-delete-window-at-right'."
  ;; split-window in batch mode doesn't work for big values of
  ;; `purpose-display-at-right-width'
  (let ((purpose-display-at-right-width 5))
    (purpose-test-delete-window-at #'purpose-display-at-right #'purpose-delete-window-at-right)))

(ert-deftest purpose-test-set-purpose ()
  "Test `purpose-set-window-purpose'"
  (save-window-excursion
    (unwind-protect
	(progn
	  (purpose-set-window-purpose 'foo)
	  (should (equal (purpose-window-purpose) 'foo)))
      (purpose-kill-buffers-safely "*pu-dummy-foo*")
      (purpose-set-window-purpose-dedicated-p nil nil))))

(ert-deftest purpose-cover-save-layout ()
  "Test that `purpose-save-window-layout' and `purpose-save-frame-layout' don't cause errors."
  (let ((filename "just-some-file"))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (split-window)
	  (purpose-save-window-layout filename)
	  (delete-file filename)
	  (purpose-save-frame-layout filename))
      (delete-file filename))))

(ert-deftest purpose-cover-load-layout ()
  "Test that `purpose-load-window-layout' and `purpose-load-frame-layout' don't cause errors."
  (let ((filename "just-some-file"))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (split-window)
	  (message "saving window layout ...")
	  (purpose-save-window-layout filename)
	  (delete-other-windows)
	  (message "loading window layout ...")
	  (purpose-load-window-layout filename)
	  (should (equal (length (window-list)) 2))
	  (message "saving frame layout ...")
	  (purpose-save-frame-layout filename)
	  (message "loading frame layout")
	  (purpose-load-frame-layout filename))
      (delete-file filename))))

(ert-deftest purpose-cover-reset-layout ()
  "Test that `purpose-reset-window-layout' and `purpose-reset-frame-layout' don't cause errors."
  (delete-other-windows)
  (split-window)
  (purpose-set-window-layout (purpose-get-window-layout))
  (delete-other-windows)
  (message "resetting window layout ...")
  (purpose-reset-window-layout)
  (should (equal (length (window-list)) 2))
  (delete-other-windows)
  (purpose-set-frame-layout (purpose-get-frame-layout))
  (message "resetting frame layout ...")
  (purpose-reset-frame-layout))

(provide 'window-purpose-test)

;;; window-purpose-test.el ends here
