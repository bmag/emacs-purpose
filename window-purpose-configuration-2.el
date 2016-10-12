;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'seq)

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
    (let ((mode (derived-mode-p purpose--compiled-mode-list)))
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

(defvar purpose-configuration nil
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
needs to be called for the changes to take effect.")

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

(defun purpose-sort-configuration ()
  "Sort `purpose-configuration' according to priority.
The entries are sorted from highest `:priority' to lowest
`:priority'. Entries with the same `:priority' are sorted by
`:name' first, `:regexp' second, and `:mode' last."
  (setq purpose-configuration
        (sort purpose-configuration #'purpose-compare-configuration-entries)))

(defun purpose--compile-modes ()
  "Compile configured modes.
Set `purpose--compiled-modes' and `purpose--compiled-mode-list'
according to `purpose-configuration'."
  (let (collected-entries collected-modes)
    (dolist (entry purpose-configuration)
      (let ((mode (plist-get entry :mode)))
        (when (and mode
                   (not (seq-some (apply-partially #'parent-mode-is-derived-p mode)
                                  collected-modes)))
          (push mode collected-modes)
          (push entry collected-entries))))
    (setq purpose--compiled-modes (nreverse collected-entries)
          purpose--compiled-mode-list (nreverse collected-modes))))

(defun purpose--compile-regexps ()
  "Compile configured regexps.
Set `purpose--compiled-regexp' according to
`purpose-configuration'."
  (let (collected-entries collected-regexps)
    (dolist (entry purpose-configuration)
      (let ((regexp (plist-get entry :regexp)))
        (when (and regexp
                   (not (member regexp collected-regexps)))
          (push regexp collected-regexps)
          (push entry collected-entries))))
    (setq purpose--compiled-regexps (nreverse collected-entries))))

(defun purpose--compile-names ()
  "Compile configured regexps.
Set `purpose--compiled-names' according to
`purpose-configuration' and `purpose--compiled-regexps'."
  (let (collected-entries collected-names)
    (dolist (entry purpose-configuration)
      (let ((name (plist-get entry :name))
            (priority (plist-get entry :primary)))
        (when (and name
                   (not (member name collected-names))
                   (not (seq-some
                         (lambda (regexp-entry)
                           (and (> (plist-get regexp-entry :priority) priority)
                                (string-match-p (plist-get regexp-entry :regexp) name)))
                         purpose--compiled-regexps)))
          (push name collected-names)
          (push entry collected-entries))))
    (setq purpose--compiled-names (nreverse collected-entries))))

(cl-defun purpose-validate-configuration (&optional (configuration purpose-configuration))
  "Throw error if CONFIGURATION is not a valid purpose configuration.
CONFIGURATION is valid if it is a list of valid purpose
configuration entries, as determined by `purpose-validate-entry'.

CONFIGURATION defaults to the value `purpose-configuration'."
  (cl-check-type configuration listp)
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
  (unless (<= 0 (plist-get entry :priority) 99)
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

(provide 'window-purpose-configuration-2)
