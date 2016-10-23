;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "purpose validation"
  (it "valid entry passes validation"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit :mode sh-mode))
            :not :to-throw))
  (it "entry must be a list"
    (expect (apply-partially #'purpose-validate-entry 0) :to-throw))
  (it "entry must contain a origin"
    (expect (apply-partially #'purpose-validate-entry
                             '(:priority 90 :purpose edit :mode sh-mode))
            :to-throw))
  (it "origin must be a symbol"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin "user" :priority 90 :purpose edit :mode sh-mode))
            :to-throw))
  (it "entry must contain a priority"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :purpose edit :mode sh-mode))
            :to-throw))
  (it "priority must be an integer"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority "90" :purpose edit :mode sh-mode))
            :to-throw))
  (it "must be true: 0 <= priority <= 99"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority -1 :purpose edit :mode sh-mode))
            :to-throw)
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 100 :purpose edit :mode sh-mode))
            :to-throw)
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 0 :purpose edit :mode sh-mode))
            :not :to-throw)
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 99 :purpose edit :mode sh-mode))
            :not :to-throw))
  (it "entry must contain a purpose"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :mode sh-mode))
            :to-throw))
  (it "purpose must be a symbol"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose "edit" :mode sh-mode))
            :to-throw))
  (it "entry must contain a name, regexp or mode"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit))
            :to-throw))
  (it "entry must contain only one of name, regexp or mode"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit :mode sh-mode :name "foo"))
            :to-throw))
  (it "name must be a string"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit :name foo))
            :to-throw))
  (it "regexp must be a string"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit :regexp foo))
            :to-throw))
  (it "mode must be a symbol"
    (expect (apply-partially #'purpose-validate-entry
                             '(:origin user :priority 90 :purpose edit :mode "sh-mode"))
            :to-throw))
  (it "configuration must be a list of entries"
    (expect (apply-partially #'purpose-validate-configuration 0) :to-throw)
    (expect (apply-partially #'purpose-validate-configuration
                             '((:origin user :priority 90 :purpose edit :mode sh-mode)
                               (:origin foo :priority 50 :purpose terminal)))
            :to-throw)
    (expect (apply-partially #'purpose-validate-configuration
                             '((:origin user :priority 90 :purpose edit :mode sh-mode)
                               (:origin foo :priority 50 :purpose terminal :regexp "^\\*shell")))
            :not :to-throw)))

(describe "Temporary Purpose Configuration"
  :var (config-snapshot default-purpose-copy base-config temp-config base-buffer other-buffer)
  (before-all
    (setq config-snapshot (get-purpose-config-2))
    (setq default-purpose-copy default-purpose)
    (setq base-config '((:origin obase :priority 80 :purpose base :name "base-buff")))
    (setq base-buffer (get-buffer-create "base-buff"))
    (setq other-buffer (get-buffer-create "other-buff")))
  (after-all
    (setq default-purpose default-purpose-copy)
    (load-purpose-config-2 config-snapshot))
  (before-each
    (setq default-purpose default-purpose-copy)
    (setq purpose-configuration base-config)
    (purpose-compile-configuration))

  (it "`purpose-save-purpose-config' restores configuration"
    (purpose-save-purpose-config
      (setq default-purpose 'not-general)
      (setq purpose-configuration nil)
      (purpose-compile-configuration))
    (expect (purpose-get-purpose base-buffer) :to-be 'base)
    (expect default-purpose :to-be 'general))
  (it "`purpose-with-temp-purposes' restores configuration"
    (purpose-with-temp-purposes :names '(("other-buff" . other))
      nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base))
  (it "`purpose-with-temp-purposes' provides new configuration"
    (purpose-with-temp-purposes :names '(("other-buff" . other))
      (expect (purpose-get-purpose other-buffer) :to-be 'other)))
  (it "`purpose-with-temp-purposes' hides old configuration"
    (purpose-with-temp-purposes :names '(("other-buff" . other))
      (expect (purpose-get-purpose base-buffer) :to-be nil)))
  (it "`purpose-with-empty-purposes' restores configuration"
    (purpose-with-empty-purposes nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base))
  (it "`purpose-with-empty-purposes' provides empty configuration"
    (purpose-with-empty-purposes
      (expect (purpose-get-purpose other-buffer) :to-be nil)))
  (it "`purpose-with-empty-purposes' hides old configuration"
    (purpose-with-empty-purposes
      (expect (purpose-get-purpose base-buffer) :to-be nil)))
  (it "`purpose-with-additional-purposes' restores configuration"
    (purpose-with-additional-purposes :names '(("other-buff" . other))
                                        nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base)
    (expect (purpose-get-purpose other-buffer) :to-be nil))
  (it "`purpose-with-additional-purposes' provides additional configuration"
    (purpose-with-additional-purposes :names '(("other-buff" . other))
      (expect (purpose-get-purpose other-buffer) :to-be 'other)))
  (it "`purpose-with-additional-purposes' doesn't hide old configuration"
    (purpose-with-additional-purposes :names '(("other-buff" . other))
      (expect (purpose-get-purpose base-buffer) :to-be 'base)))

  (it "`purpose-get-configuration-state' returns correct state"
    (setq default-purpose 'a
          purpose-configuration '(1)
          purpose--compiled-names '(2)
          purpose--compiled-regexps '(3)
          purpose--compiled-modes '(4)
          purpose--compiled-mode-list '(5))
    (expect (purpose-get-configuration-state) :to-equal
            '((default-purpose . a)
              (purpose-configuration . (1))
              (purpose--compiled-names . (2))
              (purpose--compiled-regexps . (3))
              (purpose--compiled-modes . (4))
              (purpose--compiled-mode-list . (5)))))
  (it "`purpose-set-configuration-state' sets the state correctly"
    (purpose-set-configuration-state '((purpose-configuration . (6))
                                       (purpose--compiled-names . (7))
                                       (purpose--compiled-regexps . (8))
                                       (purpose--compiled-modes . (9))
                                       (purpose--compiled-mode-list . (0))))
    (expect purpose-configuration :to-equal '(6))
    (expect purpose--compiled-names :to-equal '(7))
    (expect purpose--compiled-regexps :to-equal '(8))
    (expect purpose--compiled-modes :to-equal '(9))
    (expect purpose--compiled-mode-list :to-equal '(0))))

(describe "`purpose-compare-configuration-entries'"
  (it "sorts priority first"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 60 :purpose a :mode x-mode)
             '(:origin a :priority 40 :purpose a :name "x-name")))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 40 :purpose a :name "x-name")
             '(:origin a :priority 60 :purpose a :mode x-mode))
            :not :to-be-truthy))

  (it "sorts names above regexps"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :name "y-name")
             '(:origin a :priority 50 :purpose a :regexp "x-regexp")))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :regexp "x-regexp")
             '(:origin a :priority 50 :purpose a :name "y-name"))
            :not :to-be-truthy))
  (it "sorts regexps above modes"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :regexp "y-regexp")
             '(:origin a :priority 50 :purpose a :mode x-mode)))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :mode x-mode)
             '(:origin a :priority 50 :purpose a :regexp "y-regexp"))
            :not :to-be-truthy))
  (it "sorts names above modes"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :name "y-name")
             '(:origin a :priority 50 :purpose a :mode x-mode)))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :mode x-mode)
             '(:origin a :priority 50 :purpose a :name "y-name"))
            :not :to-be-truthy))
  (it "sorts names in lexicographical order"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :name "x-name")
             '(:origin a :priority 50 :purpose a :name "y-name")))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :name "y-name")
             '(:origin a :priority 50 :purpose a :name "x-name"))
            :not :to-be-truthy))
  (it "sorts regexps in lexicographical order"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :regexp "x-regexp")
             '(:origin a :priority 50 :purpose a :regexp "y-regexp")))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :regexp "y-regexp")
             '(:origin a :priority 50 :purpose a :regexp "x-regexp"))
            :not :to-be-truthy))
  (it "sorts modes in lexicographical order"
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :mode x-mode)
             '(:origin a :priority 50 :purpose a :mode y-mode)))
    (expect (purpose-compare-configuration-entries
             '(:origin a :priority 50 :purpose a :mode y-mode)
             '(:origin a :priority 50 :purpose a :mode x-mode))
            :not :to-be-truthy)))

(describe "Configuration Helpers"
  :var (config-snapshot)
  (before-all
    (setq config-snapshot (get-purpose-config-2)))
  (after-all
    (load-purpose-config-2 config-snapshot))
  (after-each
    (load-purpose-config-2 config-snapshot))

  (describe "`purpose-add-configuration-entry'"
    (before-each
      (setq purpose-configuration nil)
      (setq purpose--compiled-names nil))
    (it "adds entry"
      (purpose-add-configuration-entry 'test 70 'p0 :name "foo")
      (expect purpose-configuration :to-contain
              '(:origin test :priority 70 :purpose p0 :name "foo")))
    (it "compiles by default"
      (purpose-add-configuration-entry 'test 70 'p0 :name "foo")
      (expect purpose--compiled-names :to-contain '("foo" 70 p0)))
    (it "does not compile when `compilep' is nil"
      (purpose-add-configuration-entry 'test 70 'p0 :name "foo" :compilep nil)
      (expect purpose--compiled-names :not :to-contain'("foo" 70 p0))))

  (describe "`purpose-get-configuration-entry'"
    (before-each
      (setq purpose-configuration
            '((:origin test :priority 70 :purpose p0 :name "foo")
              (:origin test :priority 70 :purpose p0 :regexp "baz")
              (:origin test :priority 70 :purpose p0 :mode x-mode))))
    (it "matches if origin, priority, and name/regexp/mode are correct"
      (expect (purpose-get-configuration-entry 'test 70 :name "foo"))
      (expect (purpose-get-configuration-entry 'test 70 :regexp "baz"))
      (expect (purpose-get-configuration-entry 'test 70 :mode 'x-mode)))
    (it "doesn't match if origin is wrong"
      (expect (purpose-get-configuration-entry 'not-test 70 :name "foo") :to-be nil)
      (expect (purpose-get-configuration-entry 'not-test 70 :regexp "baz") :to-be nil)
      (expect (purpose-get-configuration-entry 'not-test 70 :mode 'x-mode) :to-be nil))
    (it "doesn't match if priority is wrong"
      (expect (purpose-get-configuration-entry 'not-test 60 :name "foo") :to-be nil)
      (expect (purpose-get-configuration-entry 'not-test 60 :regexp "baz") :to-be nil)
      (expect (purpose-get-configuration-entry 'not-test 60 :mode 'x-mode) :to-be nil))
    (it "doesn't match if name is wrong"
      (expect (purpose-get-configuration-entry 'not-test 70 :name "not-foo") :to-be nil))
    (it "doesn't match if regexp is wrong"
      (expect (purpose-get-configuration-entry 'not-test 70 :regexp "not-baz") :to-be nil))
    (it "doesn't match if mode is wrong"
      (expect (purpose-get-configuration-entry 'not-test 70 :mode 'not-x-mode) :to-be nil)))

  (describe "`purpose-delete-configuration-entry'"
    :var (name-entry regexp-entry mode-entry)
    (before-all
      (setq name-entry '(:origin test :priority 70 :purpose p0 :name "foo")
            regexp-entry '(:origin test :priority 70 :purpose p0 :regexp "baz")
            mode-entry '(:origin test :priority 70 :purpose p0 :mode x-mode)))
    (before-each
      (setq purpose-configuration
            (list name-entry regexp-entry mode-entry)))
    (it "deletes if origin, priority, and name/regexp/mode are correct"
      (purpose-delete-configuration-entry 'test 70 :name "foo")
      (expect purpose-configuration :to-equal (list regexp-entry mode-entry))
      (purpose-delete-configuration-entry 'test 70 :regexp "baz")
      (expect purpose-configuration :to-equal (list mode-entry))
      (purpose-delete-configuration-entry 'test 70 :mode 'x-mode)
      (expect purpose-configuration :to-be nil))
    (it "doesn't delete if origin is wrong"
      (purpose-delete-configuration-entry 'not-test 70 :name "foo")
      (purpose-delete-configuration-entry 'not-test 70 :regexp "baz")
      (purpose-delete-configuration-entry 'not-test 70 :mode 'x-mode)
      (expect purpose-configuration :to-equal
              (list name-entry regexp-entry mode-entry)))
    (it "doesn't delete if priority is wrong"
      (purpose-delete-configuration-entry 'test 60 :name "foo")
      (purpose-delete-configuration-entry 'test 60 :regexp "baz")
      (purpose-delete-configuration-entry 'test 60 :mode 'x-mode)
      (expect purpose-configuration :to-equal
              (list name-entry regexp-entry mode-entry)))
    (it "doesn't delete if name/regexp/mode is wrong"
      (purpose-delete-configuration-entry 'test 70 :name "not-foo")
      (purpose-delete-configuration-entry 'test 70 :regexp "not-baz")
      (purpose-delete-configuration-entry 'test 70 :mode 'not-x-mode)
      (expect purpose-configuration :to-equal
              (list name-entry regexp-entry mode-entry))))

  (describe "`purpose-add-configuration-set'"
    (before-each
      (setq purpose-configuration nil)
      (setq purpose--compiled-names nil)
      (spy-on #'purpose-add-configuration-entry)
      (spy-on #'purpose-compile-configuration))
    (it "calls `purpose-add-configuration-entry' for each entry in the set"
      (purpose-add-configuration-set 'test 70
                                     :names '(("foo" . p0) ("foo2" . p1))
                                     :regexps '(("baz" . p2))
                                     :modes '((x-mode . p3) (y-mode . p4)))
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'test 70 'p0 :name "foo" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'test 70 'p1 :name "foo2" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'test 70 'p2 :regexp "baz" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'test 70 'p3 :mode 'x-mode :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'test 70 'p4 :mode 'y-mode :compilep nil))
    (it "compiles by default"
      (purpose-add-configuration-set 'test 70
                                     :names '(("foo" . p0) ("foo2" . p1))
                                     :regexps '(("baz" . p2))
                                     :modes '((x-mode . p3) (y-mode . p4)))
      (expect #'purpose-compile-configuration :to-have-been-called))
    (it  "does not compile when `compilep' is nil"
      (purpose-add-configuration-set 'test 70
                                     :names '(("foo" . p0) ("foo2" . p1))
                                     :regexps '(("baz" . p2))
                                     :modes '((x-mode . p3) (y-mode . p4))
                                     :compilep nil)
      (expect #'purpose-compile-configuration :not :to-have-been-called)))

  (describe "`purpose-get-configuration-set'"
    :var (name-entry regexp-entry mode-entry)
    (before-all
      (setq name-entry '(:origin test :priority 70 :purpose p0 :name "foo")
            regexp-entry '(:origin test :priority 70 :purpose p0 :regexp "baz")
            mode-entry '(:origin test :priority 70 :purpose p0 :mode x-mode)))
    (before-each
      (setq purpose-configuration
            (list name-entry regexp-entry mode-entry)))
    (it "gets names"
      (expect (purpose-get-configuration-set 'test 70 :names '("foo"))
              :to-equal (list name-entry)))
    (it "gets regexps"
      (expect (purpose-get-configuration-set 'test 70 :regexps '("baz"))
              :to-equal (list regexp-entry)))
    (it "gets modes"
      (expect (purpose-get-configuration-set 'test 70 :modes '(x-mode))
              :to-equal (list mode-entry)))
    (it "gets a list of names+regexps+modes entries"
      (expect (purpose-get-configuration-set 'test 70
                                             :names '("foo")
                                             :regexps '("baz")
                                             :modes '(x-mode))
              :to-equal (list name-entry regexp-entry mode-entry))))

  (describe "`purpose-delete-configuration-set'"
    :var (name-entry regexp-entry mode-entry)
    (before-all
      (setq name-entry '(:origin test :priority 70 :purpose p0 :name "foo")
            regexp-entry '(:origin test :priority 70 :purpose p0 :regexp "baz")
            mode-entry '(:origin test :priority 70 :purpose p0 :mode x-mode)))
    (before-each
      (setq purpose-configuration
            (list name-entry regexp-entry mode-entry))
      (spy-on #'purpose-delete-configuration-entry))
    (it "calls `purpose-delete-configuration-entry' for each entry in the set"
      (purpose-delete-configuration-set 'test 70
                                        :names '("foo")
                                        :regexps '("baz")
                                        :modes '(x-mode))
      (expect #'purpose-delete-configuration-entry :to-have-been-called-with 'test 70 :name "foo")
      (expect #'purpose-delete-configuration-entry :to-have-been-called-with 'test 70 :regexp "baz")
      (expect #'purpose-delete-configuration-entry :to-have-been-called-with 'test 70 :mode 'x-mode)))
  (describe "Helper user/extension functions"
    (it "`purpose-add-user-configuration-entry' calls `purpose-add-configuration-entry'"
      (spy-on #'purpose-add-configuration-entry)
      (purpose-add-user-configuration-entry 'p0 :name "foo" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'user 99 'p0 :name "foo" :regexp nil :mode nil :compilep nil)
      (purpose-add-user-configuration-entry 'p1 :regexp "baz" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'user 99 'p1 :name nil :regexp "baz" :mode nil :compilep nil)
      (purpose-add-user-configuration-entry 'p2 :mode 'x-mode :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'user 99 'p2 :name nil :regexp nil :mode 'x-mode :compilep nil))

    (it "`purpose-add-user-configuration-set' calls `purpose-add-configuration-set'"
      (spy-on #'purpose-add-configuration-set)
      (purpose-add-user-configuration-set :names '(("foo" . p0)) :regexps '(("baz" . p1))
                                          :modes '((x-mode . p2)) :compilep nil)
      (expect #'purpose-add-configuration-set :to-have-been-called-with
              'user 99 :names '(("foo" . p0)) :regexps '(("baz" . p1))
              :modes '((x-mode . p2)) :compilep nil))

    (it "`purpose-add-extension-configuration-entry' calls `purpose-add-configuration-entry'"
      (spy-on #'purpose-add-configuration-entry)
      (purpose-add-extension-configuration-entry 'ext 'p0 :name "foo" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'ext 50 'p0 :name "foo" :regexp nil :mode nil :compilep nil)
      (purpose-add-extension-configuration-entry 'ext 'p1 :regexp "baz" :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'ext 50 'p1 :name nil :regexp "baz" :mode nil :compilep nil)
      (purpose-add-extension-configuration-entry 'ext 'p2 :mode 'x-mode :compilep nil)
      (expect #'purpose-add-configuration-entry :to-have-been-called-with
              'ext 50 'p2 :name nil :regexp nil :mode 'x-mode :compilep nil))

    (it "`purpose-add-extension-configuration-set' calls `purpose-add-configuration-set'"
      (spy-on #'purpose-add-configuration-set)
      (purpose-add-extension-configuration-set 'ext
                                               :names '(("foo" . p0)) :regexps '(("baz" . p1))
                                               :modes '((x-mode . p2)) :compilep nil)
      (expect #'purpose-add-configuration-set :to-have-been-called-with
              'ext 50 :names '(("foo" . p0)) :regexps '(("baz" . p1))
              :modes '((x-mode . p2)) :compilep nil))))

;;; TODO:
;; purpose compilation
;;; DONE:
;; purpose-compare-configuration-entries
;; purpose-add-configuration-entry
;; purpose-get-configuration-entry
;; purpose-delete-configuration-entry
;; purpose-add-configuration-set
;; purpose-get-configuration-set
;; purpose-delete-configuration-set
;; purpose-add-user-configuration-entry
;; purpose-add-extension-configuration-entry
;; purpose-add-user-configuration-set
;; purpose-add-extension-configuration-set
