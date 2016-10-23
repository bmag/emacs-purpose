;;; window-purpose-configuration.el --- Configuration handling for Purpose -*- lexical-binding: t -*-

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
;; This file contains the "purpose configuration". The "purpose
;; configuration" is a set of variables that define what is the purpose
;; of each buffer.
;;
;; To customize the purpose configuration, one should customize the variable
;; `purpose-configuration'. There exist various helper functions for modifying
;; this variable, such as `purpose-add-configuration-entry' and
;; `purpose-add-configuration-set'.
;;
;; Additionally, several functions for saving/loading the configuration and for
;; temporarily changing the configuration are provided.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'window-purpose-utils)

;;; variables

(defcustom default-purpose 'general
  "The default purpose for buffers that don't have another purpose."
  :group 'purpose
  :type 'symbol
  :package-version "1.2")

(defcustom purpose-configuration
  `((:origin default :priority 0 :purpose edit :name ".gitignore")
    (:origin default :priority 0 :purpose edit :name ".hgignore")
    ;; the `shell' command displays its buffer before setting its major-mode, so
    ;; we must detect it by name
    (:origin default :priority 0 :purpose terminal :name "*shell*")
    (:origin default :priority 0 :purpose minibuf :regexp "^ \\*Minibuf-[0-9]*\\*$")
    (:origin default :priority 0 :purpose edit :mode prog-mode)
    (:origin default :priority 0 :purpose edit :mode text-mode)
    ;; in Emacs 24.5-, `css-mode' doesn't derive from `prog-mode'
    ,@(when (version< emacs-version "25")
        '((:origin default :priority 0 :purpose edit :mode css-mode)))
    (:origin default :priority 0 :purpose terminal :mode comint-mode)
    (:origin default :priority 0 :purpose dired :mode dired-mode)
    (:origin default :priority 0 :purpose buffers :mode ibuffer-mode)
    (:origin default :priority 0 :purpose buffers :mode Buffer-menu-mode)
    (:origin default :priority 0 :purpose search :mode occur-mode)
    (:origin default :priority 0 :purpose search :mode grep-mode)
    (:origin default :priority 0 :purpose search :mode compilation-mode)
    (:origin default :priority 0 :purpose image :mode image-mode)
    (:origin default :priority 0 :purpose package :mode package-menu-mode))
  "List of all configured purposes.
Each entry is a plist with 4 keys: `:origin', `:priority',
`:purpose', and the 4th key is one of `:name', `:regexp' and
`:mode'.

`:origin' is used to identify who added the entry, so different
packages can utilize purpose-mode without overriding one
another's purposes. The value `user' is reserved for the user's
purposes, and the value `default' is reserved for the default
purposes.

`:priority' is a number between 0 and 99 (inclusive). If two
entries match a buffer, then the entry with the higher priority
is used.

`:purpose' is the purpose of matching buffers.

`:name' matches a buffer with exactly the given name.

`:regexp' matches any buffer whose name matches the given regular
expression.

`:mode' matches any buffer whose major-mode is the same as the
given mode, or derives from it.

Whenever this variable changes, `purpose-compile-configuration'
needs to be called for the changes to take effect."
  :group 'purpose
  :type '(repeat (plist :key-type (choice (const :origin)
                                          (const :priority)
                                          (const :purpose)
                                          (const :name)
                                          (const :regexp)
                                          (const :mode))
                        :value-type (choice symbol string integer)))
  :set #'(lambda (symbol value)
           (prog1 (set-default symbol value)
             (purpose-compile-configuration)))
  :initialize 'custom-initialize-default
  :package-version "2.0")

(defvar purpose--compiled-names nil
  "Compiled alist of configured names.
Each entry has the form of (name . (priority purpose)).
The alist is sorted from heighest priority to lowest priority.

Don't modify this variable directly. It should only be modified
by `purpose-compile-config'.")

(defvar purpose--compiled-regexps nil
  "Compiled alist of configured regexps.
Each entry has the form of (regexp . (priority purpose)).
The alist is sorted from heighest priority to lowest priority.

Don't modify this variable directly. It should only be modified
by `purpose-compile-config'.")

(defvar purpose--compiled-modes nil
  "Compiled alist of configured modes.
Each entry has the form of (mode . (priority purpose)).
The alist is sorted from heighest priority to lowest priority.

Don't modify this variable directly. It should only be modified
by `purpose-compile-config'.")

(defvar purpose--compiled-mode-list nil
  "Compiled list of configured modes.
The entries in this list are exactly the keys of `purpose--compiled-modes'.

Don't modify this variable directly. It should only be modified
by `purpose-compile-config'.")

;;; purpose calculation
(defun purpose-get-purpose (buffer)
  (let* ((bname (buffer-name buffer))
         ;; mode, name, regexp are nil if not found (as well as the respective
         ;; priorities and purposes)
         (mode (purpose--get-purpose-by-mode buffer))
         (m-priority (nth 1 mode))
         (m-purpose (nth 2 mode))
         (name (purpose--get-purpose-by-name bname m-priority))
         (n-priority (nth 1 name))
         (n-purpose (nth 2 name))
         (regexp (purpose--get-purpose-by-regexp bname (or n-priority m-priority)))
         (r-priority (nth 1 regexp))
         (r-purpose (nth 2 regexp)))
    ;; r-purpose is nil or larger than n-purpose
    ;; n-purpose is nil or larger or equal to m-purpose
    (or r-purpose n-purpose m-purpose)))

(defun purpose--get-purpose-by-mode (buffer)
  "Get config mode entry matching BUFFER's major mode.
Return then matching entry in `purpose--compiled-modes', or nil
if not found."
  (with-current-buffer buffer
    (let ((mode (apply #'derived-mode-p purpose--compiled-mode-list)))
      (assq mode purpose--compiled-modes))))

(defun purpose--get-purpose-by-name (buffer-name &optional min-priority)
  "Get config name entry matching BUFFER-NAME.
Only search through entries with a priority greater than or equal
to MIN-PRIORITY. If MIN-PRIORITY is nil, search all entries.

Return the matching entry in `purpose--compiled-names', or nil if
not found."
  (catch 'done
    (dolist (name purpose--compiled-names)
      (cond
       ((and min-priority
             (< (cadr name) min-priority))
        (throw 'done nil))
       ((string= (car name) buffer-name)
        (throw 'done name))))
    nil))

(defun purpose--get-purpose-by-regexp (buffer-name &optional min-priority)
  "Get config regexp entry matching BUFFER-NAME.
Only search through entries with a priority greater than
MIN-PRIORITY. If MIN-PRIORITY is nil, search all entries.

Return the matching entry in `purpose--compiled-regexps', or nil
if not found."
  (catch 'done
    (dolist (regexp purpose--compiled-regexps)
      (cond
       ((and min-priority
             (<= (cadr regexp) min-priority))
        (throw 'done nil))
       ((string-match-p (car regexp) buffer-name)
        (throw 'done regexp))))
    nil))

;;; purpose compilation

(defun purpose-compile-configuration ()
  "Compile the purpose configuration.
This function sets
`purpose--compiled-names',`purpose--compiled-regexps',
`purpose--compiled-modes', and `purpose--compiled-mode-list'
according to `purpose-configuration'."
  (purpose-validate-configuration)
  (purpose-sort-configuration)
  (purpose--compile-modes)
  (purpose--compile-regexps)
  (purpose--compile-names))

(cl-defun purpose-validate-configuration (&optional (configuration purpose-configuration))
  "Throw error if CONFIGURATION is not a valid purpose configuration.
CONFIGURATION is valid if it is a list of valid purpose
configuration entries, as determined by `purpose-validate-entry'.

CONFIGURATION defaults to the value `purpose-configuration'."
  (unless (listp configuration)
    (error "Purpose configuration must be a list"))
  (mapc #'purpose-validate-entry configuration))

(defun purpose-validate-entry (entry)
  "Throw error if ENTRY is not a valid entry for `purpose-configuration'.
See `purpose-configuration' for a description of a valid entry."
  (unless (listp entry)
    (error "Entry must be a plist: %S" entry))
  (unless (plist-get entry :origin)
    (error "Entry must contain a non-nil `:origin': %S" entry))
  (unless (symbolp (plist-get entry :origin))
    (error "`:origin' must be a symbol: %S" entry))
  (unless (plist-get entry :priority)
    (error "Entry must contain a non-nil `:priority': %S" entry))
  (unless (integerp (plist-get entry :priority))
    (error "`:priority' must be an integer: %S" entry))
  (unless (and (<= 0 (plist-get entry :priority))
               (<= (plist-get entry :priority) 99))
    (error "`:priority' must be between 0 and 99: %S" entry))
  (unless (plist-get entry :purpose)
    (error "Entry must contain a non-nil `:purpose': %S" entry))
  (unless (symbolp (plist-get entry :purpose))
    (error "`:purpose' must be a symbol: %S" entry))
  (unless (or (plist-get entry :name)
              (plist-get entry :regexp)
              (plist-get entry :mode))
    (error "Entry must contain 1 of `:name', `:regexp' or `:mode'" entry))
  (unless (= 1 (length (seq-filter (apply-partially #'plist-get entry)
                                   '(:name :regexp :mode))))
    (error "Entry must contain only 1 of `:name', `:regexp' or `:mode'" entry))
  (cond
   ((plist-get entry :name)
    (unless (stringp (plist-get entry :name))
      (error "`:name' must be a string: %S" entry)))
   ((plist-get entry :regexp)
    (unless (stringp (plist-get entry :regexp))
      (error "`:regexp' must be a string: %S" entry)))
   ((plist-get entry :mode)
    (unless (symbolp (plist-get entry :mode))
      (error "`:mode' must be a symbol: %S" entry)))))

(defun purpose-sort-configuration ()
  "Sort `purpose-configuration' according to priority.
The entries are sorted from highest `:priority' to lowest
`:priority'. Entries with the same `:priority' are sorted by
`:name' first, `:regexp' second, and `:mode' last."
  (setq purpose-configuration
        (sort purpose-configuration #'purpose-compare-configuration-entries)))

(defun purpose-compare-configuration-entries (x y)
  "Return non-nil if X has higher priority than Y."
  (cond
   ((> (plist-get x :priority) (plist-get y :priority))
    t)
   ((< (plist-get x :priority) (plist-get y :priority))
    nil)
   (t
    ;; X and Y have the same `:priority'
    (let ((x-name (plist-get x :name))
          (x-regexp (plist-get x :regexp))
          (x-mode (plist-get x :mode))
          (y-name (plist-get y :name))
          (y-regexp (plist-get y :regexp))
          (y-mode (plist-get y :mode)))
      (cond
       ;; `:name' first, sorted by `string<'
       ((and x-name
             (or (null y-name)
                 (string< x-name y-name)))
        t)
       (y-name nil)
       ;; `:regexp' second, sorted by `string<'
       ((and x-regexp
             (or (null y-regexp)
                 (string< x-regexp y-regexp)))
        t)
       (y-regexp nil)
       ;; `:mode' last, sorted by `string<' and `symbol-name'
       (t
        (string< (symbol-name x-mode) (symbol-name y-mode))))))))

(defun purpose--compile-modes ()
  "Compile configured modes.
Set `purpose--compiled-modes' and `purpose--compiled-mode-list'
according to `purpose-configuration'."
  (let (collected-entries collected-modes)
    ;; `purpose-configuration' is sorted with higher priorities coming before
    ;; lower ones. for each :mode entry, if it doesn't have a higher priority
    ;; parent (read: doesn't derive from a mode that was already collected) then
    ;; add it to the compiled variables
    (dolist (entry purpose-configuration)
      (let ((mode (plist-get entry :mode)))
        (when (and mode
                   (let ((major-mode mode))
                     (not (apply #'derived-mode-p collected-modes))))
          ;; it's a :mode entry, and doesn't derive from higher prioritized
          ;; modes
          (push mode collected-modes)
          (push (list mode (plist-get entry :priority) (plist-get entry :purpose))
                collected-entries))))
    (setq purpose--compiled-modes (nreverse collected-entries)
          purpose--compiled-mode-list (nreverse collected-modes))))

(defun purpose--compile-regexps ()
  "Compile configured regexps.
Set `purpose--compiled-regexp' according to
`purpose-configuration'."
  (let (collected-entries collected-regexps)
    ;; `purpose-configuration' is sorted with higher priorities coming before
    ;; lower ones. for each :regexp entry, if the regexp doesn't have a higher
    ;; prioritized entry (read: wasn't already collected), then add it to the
    ;; compiled variables
    (dolist (entry purpose-configuration)
      (let ((regexp (plist-get entry :regexp)))
        (when (and regexp
                   (not (member regexp collected-regexps)))
          ;; it's a :regexp entry, and it's the highest prioritized entry for
          ;; this regexp
          (push regexp collected-regexps)
          (push (list regexp (plist-get entry :priority) (plist-get entry :purpose))
                collected-entries))))
    (setq purpose--compiled-regexps (nreverse collected-entries))))

(defun purpose--compile-names ()
  "Compile configured regexps.
Set `purpose--compiled-names' according to
`purpose-configuration' and `purpose--compiled-regexps'."
  (let (collected-entries collected-names)
    ;; `purpose-configuration' is sorted with higher priorities coming before
    ;; lower ones. for each :name entry, if the name doesn't have a higher
    ;; prioritized entry (read: wasn't already collected), and if the name
    ;; doesn't match a higher prioritized regexp, then add it to the compiled
    ;; variables. because of this, `purpose--compile-names' should be called
    ;; after `purpose--compile-regexps'
    (dolist (entry purpose-configuration)
      (let ((name (plist-get entry :name))
            (priority (plist-get entry :priority)))
        (when (and name
                   (not (member name collected-names))
                   (not (seq-some
                         (lambda (regexp-entry)
                           (and (> (cadr regexp-entry) priority)
                                (string-match-p (car regexp-entry) name)))
                         purpose--compiled-regexps)))
          ;; it's a :name entry, and it's the highest prioritized entry for this
          ;; name, and this name isn't matched by a :regexp entry with a higher priority
          (push name collected-names)
          (push (list name (plist-get entry :priority) (plist-get entry :purpose))
                collected-entries))))
    (setq purpose--compiled-names (nreverse collected-entries))))

;;; functions to modify `purpose-configuration'

(cl-defun purpose-add-configuration-entry (origin priority purpose &key name regexp mode (compilep t))
  "Add new configuration entry to `purpose-configuration', and compile.
If the given paramters don't make a valid entry, throw an error
and don't change `purpose-configuration'.

If there already exist an entry with the same ORIGIN, PRIORITY,
NAME, REGEXP and MODE, then it is replaced.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (let ((new-entry (append (list :origin origin :priority priority :purpose purpose)
                           (and name (list :name name))
                           (and regexp (list :regexp regexp))
                           (and mode (list :mode mode)))))
    (purpose-validate-entry new-entry)
    (purpose-delete-configuration-entry origin priority :name name :regexp regexp :mode mode)
    (push new-entry purpose-configuration)
    (when compilep
      (purpose-compile-configuration))))

(cl-defun purpose-get-configuration-entry (origin priority &key name regexp mode)
  "Return a `purpose-configuration' entry with matching paramters.
Return nil if no entry was found."
  (seq-find (lambda (entry)
              (and (eq origin (plist-get entry :origin))
                   (= priority (plist-get entry :priority))
                   (string= name (plist-get entry :name))
                   (string= regexp (plist-get entry :regexp))
                   (eq mode (plist-get entry :mode))))
            purpose-configuration))

(cl-defun purpose-delete-configuration-entry (origin priority &key name regexp mode)
  "Remove matching configuration entry from `purpose-configuration'."
  (setq purpose-configuration
        (seq-remove (lambda (entry)
                      (and (eq origin (plist-get entry :origin))
                           (= priority (plist-get entry :priority))
                           (string= name (plist-get entry :name))
                           (string= regexp (plist-get entry :regexp))
                           (eq mode (plist-get entry :mode))))
                    purpose-configuration)))

;;; advanced helper functions for configuring `purpose-configuration'

(cl-defun purpose-add-configuration-set (origin priority &key names regexps modes (compilep t))
  "Add several configuration entries with the same ORIGIN and PRIORITY.
NAMES, REGEXPS and MODES must be alist mapping names, regexps and
modes to purposes, respectively.

If any of the entries is invalid, then `purpose-configuration' is
not changed.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (let ((original-configuration (purpose-get-configuration-state)))
    (condition-case err
        (progn
          (dolist (mode-purpose modes)
            (purpose-add-configuration-entry origin priority (cdr mode-purpose)
                                             :mode (car mode-purpose) :compilep nil))
          (dolist (regexp-purpose regexps)
            (purpose-add-configuration-entry origin priority (cdr regexp-purpose)
                                             :regexp (car regexp-purpose) :compilep nil))
          (dolist (name-purpose names)
            (purpose-add-configuration-entry origin priority (cdr name-purpose)
                                             :name (car name-purpose) :compilep nil))
          (when compilep
            (purpose-compile-configuration)))
      (error
       ;; in case of error, restore original `purpose-configuration' and
       ;; re-throw error
       (purpose-set-configuration-state)
       (signal (car err) (cdr err))))))

(cl-defun purpose-get-configuration-set (origin priority &key names regexps modes)
  "Get all configuration entries with matching parameters.
ORIGIN and PRIORITY are the same for all entries. NAMES, REGEXPS
and MODES are lists of names, regexps and modes, respectively."
  (delq nil
        (append
         (mapcar (apply-partially #'purpose-get-configuration-entry origin priority :name) names)
         (mapcar (apply-partially #'purpose-get-configuration-entry origin priority :regexp) regexps)
         (mapcar (apply-partially #'purpose-get-configuration-entry origin priority :mode) modes))))

(cl-defun purpose-delete-configuration-set (origin priority &key names regexps modes)
  "Delete all matching configuration entries.
ORIGIN and PRIORITY are the same for all entries. NAMES, REGEXPS
and MODES are lists of names, regexps and modes, respectively."
  (mapc (apply-partially #'purpose-delete-configuration-entry origin priority :name) names)
  (mapc (apply-partially #'purpose-delete-configuration-entry origin priority :regexp) regexps)
  (mapc (apply-partially #'purpose-delete-configuration-entry origin priority :mode) modes))

(cl-defun purpose-add-user-configuration-entry (purpose &key name regexp mode (compilep t))
  "Add new user configuration entry to `purpose-configuration'.
A user configuration entry is a regular entry, with an origin of
`user' and a priority of 99. See
`purpose-add-configuration-entry' for details.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (purpose-add-configuration-entry 'user 99 :name name :regexp regexp
                                   :mode mode :compilep compilep))

(cl-defun purpose-add-extension-configuration-entry (origin purpose &key name regexp mode (compilep t))
  "Add new extension configuration entry to `purpose-configuration'.
A extension configuration entry is a regular entry, with a
priority of 50. See `purpose-add-configuration-entry' for
details.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (purpose-add-configuration-entry origin 50 :name name :regexp regexp
                                   :mode mode :compilep compilep))

(cl-defun purpose-add-user-configuration-set (purpose &key names regexps modes (compilep t))
  "Add several user configuration entries to `purpose-configuration'.
A user configuration entry is a regular entry, with an origin of
`user' and a priority of 99. See `purpose-add-configuration-set'
for details.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (purpose-add-configuration-set 'user 99 :names names :regexps regexps
                                 :modes modes :compilep compilep))

(cl-defun purpose-add-extension-configuration-set (origin &key names regexps modes (compilep t))
  "Add several extension configuration entries to `purpose-configuration'.
A extension configuration entry is a regular entry, with a priority of 50.
See `purpose-add-configuration-set' for details.

If COMPILEP is non-nil, then also compile the configuration. The
default is non-nil."
  (purpose-add-configuration-set origin 50 :names names :regexps regexps
                                 :modes modes :compilep compilep))

;;; save/load configuration state

(defconst purpose--configuration-state-vars
  '(default-purpose
    purpose-configuration
    purpose--compiled-names
    purpose--compiled-regexps
    purpose--compiled-modes
    purpose--compiled-mode-list))

(defun purpose-get-configuration-state ()
  "Return the state of the current purpose configuration.
The purpose configuration consists of the variables listed in
`purpose--configuration-state-vars'."
  (mapcar (lambda (var)
            (let ((value (symbol-value var)))
              (cons var (if (sequencep value)
                            (copy-sequence value)
                          value))))
          purpose--configuration-state-vars))

(defun purpose-set-configuration-state (state)
  "Load state of purpose configuration from STATE.
This changes the values of the variables listed in
`purpose--configuration-state-vars'."
  (dolist (var purpose--configuration-state-vars)
    (set var (cdr (assq var state)))))

;;; change purposes temporarily

(defmacro purpose-save-purpose-config (&rest body)
  "Save the purpose configuration, execute BODY, restore the configuration."
  (declare (indent defun) (debug body))
  `(let ((default-purpose default-purpose)
         (purpose--compiled-names purpose--compiled-names)
         (purpose--compiled-regexps purpose--compiled-regexps)
         (purpose--compiled-modes purpose--compiled-modes)
         (purpose--compiled-mode-list purpose--compiled-mode-list)
         (purpose-configuration purpose-configuration))
     ,@body))

(defmacro purpose-with-temp-purposes (&rest body)
  "Execute BODY with a temporary purpose configuration.
ORIGIN, PRIORITY, NAMES, REGEXPS and MODES have the same meaning
as in `purpose-add-configuration-set'. ORIGIN defaults to `temp'
and PRIORITY defaults to 99. The purpose configuration is
restored after BODY is executed.

\(fn &key ORIGIN PRIORITY NAMES REGEXPS MODES &rest BODY)"
  (declare (indent defun)
           (debug ([&rest keywordp sexp] body)))
  (destructuring-bind (keys body)
      (purpose-pop-keys '((:origin 'temp) (:priority 99)
                          :names :regexps :modes)
                        body)
    `(purpose-save-purpose-config
       (setq purpose-configuration nil)
       (purpose-add-configuration-set ,(plist-get keys :origin)
                                      ,(plist-get keys :priority)
                                      :names ,(plist-get keys :names)
                                      :regexps ,(plist-get keys :regexps)
                                      :modes ,(plist-get keys :modes))
       ,@body)))

(defmacro purpose-with-additional-purposes (&rest body)
  "Execute BODY with an addiaitional purpose configuration.
ORIGIN, PRIORITY, NAMES, REGEXPS and MODES have the same meaning
as in `purpose-add-configuration-set'. ORIGIN defaults to `temp'
and PRIORITY defaults to 99. The purpose configuration is
restored after BODY is executed.

\(fn &key ORIGIN PRIORITY NAMES REGEXPS MODES &rest BODY)"
  (declare (indent defun)
           (debug ([&rest keywordp sexp] body)))
  (destructuring-bind (keys body)
      (purpose-pop-keys '((:origin 'temp) (:priority 99)
                          :names :regexps :modes)
                        body)
    `(purpose-save-purpose-config
       (purpose-add-configuration-set ,(plist-get keys :origin)
                                      ,(plist-get keys :priority)
                                      :names ,(plist-get keys :names)
                                      :regexps ,(plist-get keys :regexps)
                                      :modes ,(plist-get keys :modes))
       ,@body)))

(defmacro purpose-with-empty-purposes (&rest body)
  "Execute BODY with an empty purpose configuration.
The purpose configuration is restored after BODY is executed."
  (declare (indent defun) (debug body))
  `(purpose-with-temp-purposes ,@body))

;; set initial state of compiled variables (`purpose--compiled-*') according to
;; initial state of `purpose-configuration'
(purpose-compile-configuration)

;;; TODO:
;; - tests
;;; DONE:
;; - equivalents to `purpose-save-purpose-config', `purpose-with-temp-purposes',
;;   `purpose-with-empty-purposes' and `purpose-with-additional-purposes'.
;; - initial configuration (including default entires)
;; - add default entries to `purpose-configuration' (make it not empty by default)
;; - convert `defvar's to `defcustom's.
;; - helpers function should compile unless told otherwise
;; - use a real pair of load/save functions to restore all config variables upon error
;; - rename all *-2 functions/variables to remove the suffix

(provide 'window-purpose-configuration)

;;; window-purpose-configuration.el ends here
