;; -*- lexical-binding: t -*-
(require 'buttercup-init)
(require 'window-purpose-configuration-2)

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
  :var (config-snapshot base-config temp-config base-buffer other-buffer)
  (before-all
    (setq config-snapshot (get-purpose-config-2))
    (setq base-config '((:origin obase :priority 80 :purpose base :name "base-buff")))
    (setq base-buffer (get-buffer-create "base-buff"))
    (setq other-buffer (get-buffer-create "other-buff")))
  (after-all
    (load-purpose-config-2 config-snapshot))
  (before-each
    (setq purpose-configuration base-config)
    (purpose-compile-configuration))

  (it "`purpose-save-purpose-config' restores configuration"
    (purpose-save-purpose-config-2
      (setq purpose-configuration nil)
      (purpose-compile-configuration))
    (expect (purpose-get-purpose base-buffer) :to-be 'base))
  (it "`purpose-with-temp-purposes' restores configuration"
    (purpose-with-temp-purposes-2 :names '(("other-buff" . other))
      nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base))
  (it "`purpose-with-temp-purposes' provides new configuration"
    (purpose-with-temp-purposes-2 :names '(("other-buff" . other))
      (expect (purpose-get-purpose other-buffer) :to-be 'other)))
  (it "`purpose-with-temp-purposes' hides old configuration"
    (purpose-with-temp-purposes-2 :names '(("other-buff" . other))
      (expect (purpose-get-purpose base-buffer) :to-be nil)))
  (it "`purpose-with-empty-purposes' restores configuration"
    (purpose-with-empty-purposes-2 nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base))
  (it "`purpose-with-empty-purposes' provides empty configuration"
    (purpose-with-empty-purposes-2
      (expect (purpose-get-purpose other-buffer) :to-be nil)))
  (it "`purpose-with-empty-purposes' hides old configuration"
    (purpose-with-empty-purposes-2
      (expect (purpose-get-purpose base-buffer) :to-be nil)))
  (it "`purpose-with-additional-purposes' restores configuration"
    (purpose-with-additional-purposes-2 :names '(("other-buff" . other))
                                        nil)
    (expect (purpose-get-purpose base-buffer) :to-be 'base)
    (expect (purpose-get-purpose other-buffer) :to-be nil))
  (it "`purpose-with-additional-purposes' provides additional configuration"
    (purpose-with-additional-purposes-2 :names '(("other-buff" . other))
      (expect (purpose-get-purpose other-buffer) :to-be 'other)))
  (it "`purpose-with-additional-purposes' doesn't hide old configuration"
    (purpose-with-additional-purposes-2 :names '(("other-buff" . other))
      (expect (purpose-get-purpose base-buffer) :to-be 'base))))
