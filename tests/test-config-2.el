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
