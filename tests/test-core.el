;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(defvar test-purpose-config
  `((purpose-user-name-purposes . (("FOO" . USER1)))
    (purpose-user-regexp-purposes . (("^FOO2\\'" . USER2)))
    (purpose-user-mode-purposes . ((text-mode . USER3)))
    (purpose-extended-configuration . (:test
                                       ,(purpose-conf "test-conf"
                                                      :name-purposes '(("BAR" . EXT1))
                                                      :regexp-purposes '(("^BAR2\\'" . EXT2))
                                                      :mode-purposes '((sh-mode . EXT3)))))
    (purpose-use-default-configuration . t)))

(describe "Core Suite"
  :var (config-suite-snapshot config-case-snapshot original-layout-dirs)
  (before-all
    (purpose-mode)
    (setq config-suite-snapshot (get-purpose-config)))

  (after-all
    (load-purpose-config config-suite-snapshot)
    (purpose-mode -1))

  (before-each
    (setq config-case-snapshot (get-purpose-config))
    (load-purpose-config test-purpose-config)
    (create-buffers "FOO" "FOO2" "FOO3"
                    "BAR" "BAR2" "BAR3"
                    ".gitignore" " *Minibuf-777*" "TUX"
                    "*pu-dummy-DUMMY*" "xxx")
    (with-current-buffer "FOO3" (text-mode))
    (with-current-buffer "BAR3" (sh-mode))
    (with-current-buffer "TUX" (emacs-lisp-mode))
    (with-current-buffer "xxx" (fundamental-mode))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (set-window-dedicated-p nil nil)
    (purpose-set-window-purpose-dedicated-p nil nil)
    (set-window-buffer nil "xxx"))

  (after-each
    (load-purpose-config config-suite-snapshot))

  ;; user/ext/default, name/regexp/mode, dummy, default-purpose
  (describe "purpose-buffer-purpose"
    (it "gets user-name purpose"
      (expect (purpose-buffer-purpose "FOO") :to-be 'USER1))
    (it "gets user-regexp purpose"
      (expect (purpose-buffer-purpose "FOO2") :to-be 'USER2))
    (it "gets user-mode purpose"
      (expect (purpose-buffer-purpose "FOO3") :to-be 'USER3))
    (it "gets ext-name purpose"
      (expect (purpose-buffer-purpose "BAR") :to-be 'EXT1))
    (it "gets ext-regexp purpose"
      (expect (purpose-buffer-purpose "BAR2") :to-be 'EXT2))
    (it "gets ext-mode purpose"
      (expect (purpose-buffer-purpose "BAR3") :to-be 'EXT3))
    (it "gets default-name purpose"
      (expect (purpose-buffer-purpose ".gitignore") :to-be 'edit))
    (it "gets default-regexp purpose"
      (expect (purpose-buffer-purpose " *Minibuf-777*") :to-be 'minibuf))
    (it "gets default-mode purpose"
      (expect (purpose-buffer-purpose "TUX") :to-be 'edit))
    (it "gets dummy purpose"
      (expect (purpose-buffer-purpose "*pu-dummy-DUMMY*") :to-be 'DUMMY))
    (it "gets default purpose"
      (expect (purpose-buffer-purpose "xxx") :to-be default-purpose)))

  (describe "purpose-buffers-with-purpose"
    (it "finds all buffers with given purpose"
      (expect (purpose-buffers-with-purpose 'USER1) :to-equal (list (get-buffer "FOO")))))
  (describe "purpose-window-purpose"
    (it "gets purpose of given window"
      (set-window-buffer nil "FOO")
      (expect (purpose-window-purpose) :to-be 'USER1)))
  (describe "purpose-windows-with-purpose"
    (it "finds all windows with given purpose"
      (set-window-buffer (split-window) "FOO2")
      (set-window-buffer nil "FOO")
      (expect (purpose-windows-with-purpose 'USER1) :to-equal (list (selected-window)))))
  (describe "purpose-get-all-purposes"
    (it "finds all purposes that are defined"
      (expect (cl-sort (purpose-get-all-purposes) #'string< :key #'symbol-name)
              :to-equal
              '(EXT1 EXT2 EXT3 USER1 USER2 USER3 buffers dired edit general
                     image minibuf package search terminal))))

  (describe "purpose-read-purpse"
    (it "reads a purpose from the user"
      (insert-user-input "EXT3")
      (expect (purpose-read-purpose "Purpose: ") :to-be 'EXT3)))

  (describe "purpose--get-buffer-create"
    (before-each
      (when (get-buffer "newbuf") (kill-buffer "*pu-dummy-BLAH*")))
    (after-each
      (when (get-buffer "newbuf") (kill-buffer "*pu-dummy-BLAH*")))
    (it "returns a purpose's buffer when one exists"
      (expect (purpose--get-buffer-create 'USER1) :to-be (get-buffer "FOO")))
    (it "creates a new buffer when purpose has no buffers"
      (expect (purpose--get-buffer-create 'BLAH) :to-be (get-buffer "*pu-dummy-BLAH*"))
      (expect (get-buffer "*pu-dummy-BLAH*") :to-be-truthy)))

  (describe "purpose--set-window-buffer"
    (before-each
      (when (get-buffer "newbuf") (kill-buffer "*pu-dummy-BLAH*")))
    (after-each
      (when (get-buffer "newbuf") (kill-buffer "*pu-dummy-BLAH*")))
    (it "returns a purpose's buffer when one exists"
      (purpose--set-window-buffer 'USER1)
      (expect '(:name "FOO" :purpose USER1) :to-match-window-tree))
    (it "creates a new buffer when purpose has no buffers"
      (purpose--set-window-buffer 'BLAH)
      (expect '(:name "*pu-dummy-BLAH*" :purpose BLAH) :to-match-window-tree)))

  (describe "purpose/buffer dedication togglers"
    (before-each
      (spy-on 'message :and-call-through))
    (it "purpose-set-window-purpose-dedicated-p changes purpose dedication status"
      (purpose-set-window-purpose-dedicated-p nil t)
      (expect (window-parameter nil 'purpose-dedicated) :to-be t)
      (purpose-set-window-purpose-dedicated-p nil nil)
      (expect (window-parameter nil 'purpose-dedicated) :to-be nil))
    (it "purpose-window-purpose-dedicated-p reads purpose dedication status"
      (purpose-set-window-purpose-dedicated-p nil t)
      (expect (purpose-window-purpose-dedicated-p) :to-be t)
      (purpose-set-window-purpose-dedicated-p nil nil)
      (expect (purpose-window-purpose-dedicated-p) :to-be nil))
    (it "purpose-toggle-window-purpose-dedicated toggles purpose dedication status"
      (purpose-set-window-purpose-dedicated-p nil nil)
      (purpose-toggle-window-purpose-dedicated)
      (expect (purpose-window-purpose-dedicated-p) :to-be t)
      (expect 'message :to-have-been-called-with "Window purpose is now dedicated")
      (purpose-toggle-window-purpose-dedicated)
      (expect (purpose-window-purpose-dedicated-p) :to-be nil)
      (expect 'message :to-have-been-called-with "Window purpose is not dedicated anymore"))
    (it "purpose-toggle-window-buffer-dedicated toggles buffer dedication status"
      (set-window-dedicated-p nil nil)
      (purpose-toggle-window-buffer-dedicated)
      (expect (window-dedicated-p) :to-be t)
      (expect 'message :to-have-been-called-with "Window buffer is now dedicated")
      (purpose-toggle-window-buffer-dedicated)
      (expect (window-dedicated-p) :to-be nil)
      (expect 'message :to-have-been-called-with "Window buffer is not dedicated anymore")))

  (describe "purpose-get-*-window Functions"
    (describe "purpose-get-top-window"
      (it "deletes top window"
        (set-window-buffer (split-window nil nil 'above) "TUX")
        (expect (purpose-get-top-window) :window-to-contain "TUX")))

    (describe "purpose-get-bottom-window"
      (it "deletes bottom window"
        (set-window-buffer (split-window nil nil 'below) "TUX")
        (expect (purpose-get-bottom-window) :window-to-contain "TUX")))

    (describe "purpose-get-left-window"
      (it "deletes left window"
        (set-window-buffer (split-window nil nil 'left) "TUX")
        (expect (purpose-get-left-window) :window-to-contain "TUX")))

    (describe "purpose-get-right-window"
      (it "deletes right window"
        (set-window-buffer (split-window nil nil 'right) "TUX")
        (expect (purpose-get-right-window) :window-to-contain "TUX")))))
