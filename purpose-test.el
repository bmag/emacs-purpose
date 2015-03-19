(when (require 'undercover nil t)
  (undercover "purpose.el")
  (undercover "purpose-configuration.el")
  (undercover "purpose-core.el")
  (undercover "purpose-fixes.el")
  (undercover "purpose-layout.el")
  (undercover "purpose-prefix-overload.el")
  (undercover "purpose-switch.el")
  (undercover "purpose-x.el"))

(require 'purpose)
(require 'purpose-x)

;;; example

(ert-deftest purpose-test-dummy-buffer-name ()
  "Test generation of dummy buffer names"
  (should (equal (purpose--dummy-buffer-name 'edit) "*pu-dummy-edit*")))



;;; purpose-configuration.el

(ert-deftest purpose-test-compile-user-config ()
  "Test compilation of user configuration.
This tests that `purpose-compile-user-configuration' creates the correct
hash tables for the uncompiled alists."
  (let ((purpose--user-mode-purposes (make-hash-table))
	(purpose--user-name-purposes (make-hash-table :test #'equal))
	(purpose--user-regexp-purposes (make-hash-table :test #'equal)))
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
