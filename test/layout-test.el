;;; layout-test.el --- Tests for window-purpose-layout.el -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Bar Magal

;; Author: Bar Magal
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
;; This file contains tests for window-purpose-layout.el

;;; Code:
(ert-deftest purpose-test-delete-non-dedicated-windows ()
  "Test `purpose-delete-non-dedicated-windows'."
  (save-window-excursion
    (delete-other-windows)
    (let ((window (selected-window)))
      (purpose-set-window-purpose-dedicated-p window t)
      (split-window window)
      (purpose-delete-non-dedicated-windows)
      (should (equal (window-list) (list window))))))

(ert-deftest purpose-test-delete-window-at-top ()
  "Test `purpose-delete-window-at-top'."
  (purpose-test-delete-window-at #'purpose-display-at-top #'purpose-delete-window-at-top))

(ert-deftest purpose-test-set-purpose ()
  "Test `purpose-set-window-purpose'"
  (save-window-excursion
    (unwind-protect
        (progn
          (purpose-set-window-purpose 'foo)
          (should (equal (purpose-window-purpose) 'foo)))
      (purpose-kill-buffers-safely "*pu-dummy-foo*")
      (purpose-set-window-purpose-dedicated-p nil nil))))

(ert-deftest purpose-cover-save-layout-file ()
  "Test that `purpose-save-window-layout-file' and `purpose-save-frame-layout-file' don't cause errors."
  (let ((filename "just-some-file"))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (purpose-save-window-layout-file filename)
          (delete-file filename)
          (purpose-save-frame-layout-file filename))
      (ignore-errors (delete-file filename)))))

(ert-deftest purpose-cover-save-layout ()
  "Test that `purpose-save-window-layout' and `purpose-save-frame-layout' don't cause errors."
  (let* ((layout-name "just-some-layout")
         (layout-dir (car purpose-layout-dirs))
         (window-filename (concat (file-name-as-directory layout-dir)
                                  layout-name ".window-layout"))
         (frame-filename (concat (file-name-as-directory layout-dir)
                                  layout-name ".frame-layout")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (purpose-save-window-layout layout-name layout-dir)
          (should (file-exists-p window-filename))
          (delete-file window-filename)
          (purpose-save-frame-layout layout-name layout-dir)
          (should (file-exists-p frame-filename))
          (delete-file frame-filename))
      (ignore-errors (delete-file window-filename))
      (ignore-errors (delete-file frame-filename)))))

(ert-deftest purpose-cover-load-layout-file ()
  "Test that `purpose-load-window-layout-file' and `purpose-load-frame-layout-file' don't cause errors."
  (let ((filename "just-some-file"))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (message "saving window layout ...")
          (purpose-save-window-layout-file filename)
          (delete-other-windows)
          (message "loading window layout ...")
          (purpose-load-window-layout-file filename)
          (should (equal (length (window-list)) 2))
          (message "saving frame layout ...")
          (purpose-save-frame-layout-file filename)
          (message "loading frame layout")
          (purpose-load-frame-layout-file filename))
      (ignore-errors (delete-file filename)))))

(ert-deftest purpose-test-load-layout ()
  "Test that `purpose-load-window-layout' and `purpose-load-frame-layout-file' load a layout correctly."
  (let ((layout-in-dir1 "dired2")
        (layout-in-dir2 "dired-edit-general")
        (layout-in-both-dirs "edit-terminal")
        (frame-layout "edit-terminal"))
    (message "loading window layout from lower priority directory ...")
    (purpose-load-window-layout layout-in-dir1)
    (should (equal (mapcar #'purpose-window-purpose (window-list))
                   '(dired dired)))
    (message "loading window layout from higher priority directory ...")
    (purpose-load-window-layout layout-in-dir2)
    (should (equal (mapcar #'purpose-window-purpose (window-list))
                   '(dired edit general)))
    (message "loading window layout from both directories (should prefer higher priority dir) ...")
    (purpose-load-window-layout layout-in-both-dirs)
    ;; layout from lower priority dir produces '(edit edit terminal) instead
    (should (equal (mapcar #'purpose-window-purpose (window-list))
                   '(edit terminal)))
    (message "loading frame layout ...")
    ;; frame-layout should only contain one frame, so no need to delete frames afterwards
    (purpose-load-frame-layout frame-layout)
    (should (equal (mapcar #'purpose-window-purpose (window-list))
                   '(edit edit terminal terminal)))))

(ert-deftest purpose-test-interactive-save-window-layout ()
  "Test interactive saving and loading of window layout."
  (let ((filename "just-another-file")
        (stored-layout nil))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (setq stored-layout (purpose-get-window-layout))
          (should stored-layout)
          (purpose-insert-user-input filename)
          (call-interactively 'purpose-save-window-layout-file)
          (should (file-exists-p filename))
          (delete-other-windows)
          (purpose-insert-user-input filename)
          (call-interactively 'purpose-load-window-layout-file)
          (should (equal stored-layout (purpose-get-window-layout))))
      (ignore-errors (delete-file filename)))))

(ert-deftest purpose-test-interactive-save-frame-layout ()
  "Test interactive saving and loading of window layout."
  (let ((filename "just-another-file")
        (stored-layout nil))
    (unwind-protect
        (progn
          (delete-other-frames)
          (delete-other-windows)
          (split-window)
          (setq stored-layout (purpose-get-frame-layout))
          (should stored-layout)
          (purpose-insert-user-input filename)
          (call-interactively 'purpose-save-frame-layout-file)
          (should (file-exists-p filename))
          (delete-other-windows)
          (purpose-insert-user-input filename)
          (call-interactively 'purpose-load-frame-layout-file)
          (should (equal stored-layout (purpose-get-frame-layout))))
      (ignore-errors (delete-file filename)))))

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

(ert-deftest purpose-test-set-window-purpose ()
  "Test that `purpose-set-window-purpose' does set the purpose."
  (unwind-protect
      (purpose-with-temp-config
          '((foo-mode . purp1) (bar-mode . purp2))
          nil
          nil
        (purpose-insert-user-input "purp1")
        (purpose-call-with-prefix-arg t 'purpose-set-window-purpose)
        (should (equal (purpose-window-purpose) 'purp1))
        (should-not (purpose-window-purpose-dedicated-p))
        (purpose-insert-user-input "purp2")
        (call-interactively 'purpose-set-window-purpose)
        (should (equal (purpose-window-purpose) 'purp2))
        (should (purpose-window-purpose-dedicated-p))
        (purpose-insert-user-input "purp1")
        (purpose-call-with-prefix-arg t 'purpose-set-window-purpose)
        (should (equal (purpose-window-purpose) 'purp1))
        (should-not (purpose-window-purpose-dedicated-p)))
    (delete-other-windows)
    (set-window-dedicated-p nil nil)
    (purpose-set-window-purpose-dedicated-p nil nil)
    (purpose-kill-buffers-safely "*pu-dummy-purp1*" "*pu-dummy-purp2*")))

(provide 'layout-test)

;;; layout-test.el ends here
