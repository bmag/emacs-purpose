;; (when (require 'undercover nil t)
;;   (setq undercover-force-coverage t)
;;   ;; (undercover "purpose.el")
;;   (undercover "purpose-configuration.el" (:report-file "/tmp/undercover-report.json"))
;;   ;; (undercover "purpose-core.el")
;;   ;; (undercover "purpose-fixes.el")
;;   ;; (undercover "purpose-layout.el")
;;   ;; (undercover "purpose-prefix-overload.el")
;;   ;; (undercover "purpose-switch.el")
;;   ;; (undercover "purpose-x.el")
;;   )

(require 'purpose)
(require 'purpose-x)



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
   (should (equal (hash-table-keys purpose--default-regexp-purposes) nil))
   (should (equal (hash-table-values purpose--default-regexp-purposes) nil))))



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

;;; TODO: toggle-window-buffer-dedicated, toggle-window-purpose-dedicated
;;; TODO: get-top/bottom/left/right-window

(ert-deftest purpose-test-buffers-with-purpose ()
  "Test `purpose-buffers-with-purpose'."
  (purpose-with-temp-config
   nil nil '(("xxx-test" . test))
   (unwind-protect
       (let (buffers)
	 (add-to-list 'buffers (get-buffer-create "xxx-test-1"))
	 (add-to-list 'buffers (get-buffer-create "xxx-test-2"))
	 (get-buffer-create "another-buffer")
	 (should (or (equal (purpose-buffers-with-purpose 'test) buffers)
		     (equal (purpose-buffers-with-purpose 'test) (reverse buffers)))))
     (let ((kill-buffer-query-functions nil)
	   (kill-buffer-hook nil))
       (ignore-errors (kill-buffer "xxx-test-1"))
       (ignore-errors (kill-buffer "xxx-test-2"))
       (ignore-errors (kill-buffer "another-buffer"))))))

(ert-deftest purpose-test-window-purpose ()
  "Test `purpose-window-purpose'."
  (unwind-protect
      (save-window-excursion
	(set-window-buffer nil (get-buffer-create "xxx-test-1"))
	(purpose-with-temp-config
	 nil '(("xxx-test-1" . test)) nil
	 (should (equal (purpose-window-purpose) 'test))))
    (let ((kill-buffer-query-functions nil)
	  (kill-buffer-hook nil))
      (ignore-errors (kill-buffer "xxx-test-1")))))

(ert-deftest purpose-test-windows-with-purpose ()
  "Test `purpose-windows-with-purpose'."
  (unwind-protect
      (save-window-excursion
	(let (windows
	      (split-width-threshold 1)
	      (split-height-threshold 1))
	  (delete-other-windows)
	  (set-window-buffer nil (get-buffer-create "xxx-test-1"))
	  (add-to-list 'windows (selected-window))
	  (select-window (split-window))
	  (set-window-buffer nil (get-buffer-create "another-buffer"))
	  (purpose-with-temp-config
	   nil '(("another-buffer" . foo)) '(("xxx-test" . test))
	   (should (equal (purpose-windows-with-purpose 'test) windows)))))
    (let ((kill-buffer-query-functions nil)
	  (kill-buffer-hook nil))
      (mapc #'(lambda (buf) (ignore-errors (kill-buffer buf)))
	    '("xxx-test-1" "xxx-test-2" "another-buffer")))))



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

