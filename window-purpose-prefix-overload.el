;;; window-purpose-prefix-overload.el --- Bind several commands to the same key -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Bar Magal & contributors

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
;; This file contains functions and macros for using the same
;; key-binding for several commands (overloading). The correct command
;; is chosen by considering the prefix argument.
;;
;; For example, this binds `find-file' and
;; `find-file-without-purpose' to C-x C-f:
;;    (def-prefix-overload purpose-find-file-overload
;;      '(find-file find-file-without-purpose))
;;    (define-key purpose-mode-map (kbd "C-x C-f")
;;      #'purpose-find-file-overload)
;; To call `find-file', the user presses C-x C-f. To call
;; `find-file-without-purpose', the user presses C-u C-x C-f.

;;; Code:

(require 'cl-lib)

(defun purpose--prefix-arg-to-index (prefix-argument)
  "Turn prefix argument PREFIX-ARGUMENT to a logical index.
Examples:
C-u <command>: index 1
C-u C-u <command>: index 2
C-u 2 <command>: index 2
C-u 1 2 <command>: index 12
<command>: index 0 (no prefix argument used)"
  (cond
   ((null prefix-argument)
    0)

   ((listp prefix-argument)
    (round (log (car prefix-argument) 4)))

   ((eq prefix-argument '-)
    -1)

   (t
    prefix-argument)))

(defun purpose--generate-documentation-def-prefix-overload (name commands)
  (let ((doc-first (format "\\[%s]: `%s'" name (car commands)))
        (doc-rest
         (cl-loop for c in (cdr commands)
                  for i from 1
                  collect (format "%s \\[%s], C-u %s \\[%s]: `%s'"
                                  (mapconcat #'identity
                                             (cl-loop for j from 1 to i
                                                      collect "C-u")
                                             " ")
                                  name
                                  i
                                  name
                                  c))))
    (mapconcat
     #'identity
     (append
      (list "This function was generated by `define-purpose-prefix-overload'."
            ""
            doc-first)
      doc-rest)
     "\n")))

(defmacro define-purpose-prefix-overload (name commands)
  "Define an interactive function named NAME, which calls interactively
one command from COMMANDS.
The command is chosen by the prefix argument:
no prefix argument: first command;
C-u or C-u 1: second command;
C-u C-u or C-u 2: third command;
and so on.
Use it like this:
   (define-purpose-prefix-overload hello \\='(command1 command2 command3))"
  (declare (indent defun) (debug (&define name (&rest sexp))))
  (unless (eval commands)
    (error "Argument COMMANDS cannot be empty"))
  `(defun ,name (&optional arg)
     ,(purpose--generate-documentation-def-prefix-overload name (eval commands))
     (interactive "P")
     (let* ((index (purpose--prefix-arg-to-index arg))
            (command (nth index ,commands)))
       (if command
           (call-interactively command)
         (error "Index %s too big" index)))))

(provide 'window-purpose-prefix-overload)
;;; window-purpose-prefix-overload.el ends here
