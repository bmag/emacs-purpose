;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "define-purpose-prefix-overload"
  (before-all
    (define-purpose-prefix-overload --purpose-prefix-test
      '((lambda () (interactive) 0)
        (lambda () (interactive) 1)
        (lambda () (interactive) 2)
        (lambda () (interactive) 3))))
  (it "defines all numeric prefix args correctly"
    (expect (purpose-call-with-prefix-arg nil '--purpose-prefix-test) :to-be 0)
    (expect (purpose-call-with-prefix-arg 0 '--purpose-prefix-test) :to-be 0)
    (expect (purpose-call-with-prefix-arg 1 '--purpose-prefix-test) :to-be 1)
    (expect (purpose-call-with-prefix-arg 2 '--purpose-prefix-test) :to-be 2)
    (expect (purpose-call-with-prefix-arg 3 '--purpose-prefix-test) :to-be 3))
  (it "throws error on undefined numeric prefix arg"
    (expect (apply-partially 'purpose-call-with-prefix-arg 4 '--purpose-prefix-test) :to-throw))
  (it "defines all C-u prefix args correctly"
    (expect (purpose-call-with-prefix-arg '(4) '--purpose-prefix-test) :to-be 1)
    (expect (purpose-call-with-prefix-arg '(16) '--purpose-prefix-test) :to-be 2)
    (expect (purpose-call-with-prefix-arg '(64) '--purpose-prefix-test) :to-be 3))
  (it "throws error on undefined C-u prefix arg"
    (expect (apply-partially 'purpose-call-with-prefix-arg '(256) '--purpose-prefix-test) :to-throw)))
