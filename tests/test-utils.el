;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "purpose-message-on-p off"
  :var (enabledp)
  (before-all (setq enabledp purpose-message-on-p
                    purpose-message-on-p nil))
  (after-all (setq purpose-message-on-p enabledp))
  (it "returns formatted message"
    (expect (purpose-message "We %s good" "are") :to-equal "We are good"))
  (it "doesn't emit message"
    (spy-on #'message)
    (purpose-message "Hello")
    (expect #'message :not :to-have-been-called)))

(describe "purpose-message-on-p on"
  :var (enabledp)
  (before-all (setq enabledp purpose-message-on-p
                    purpose-message-on-p t))
  (after-all (setq purpose-message-on-p enabledp))
  (it "returns formatted message"
    (expect (purpose-message "We %s good" "are") :to-equal "We are good"))
  (it "emits message"
    (spy-on #'message)
    (purpose-message "Hello")
    (expect #'message :to-have-been-called)))

(describe "purpose-alist-get"
  (it "finds existing entry"
    (expect (purpose-alist-get 'b '((a . 1) (b . 2)) :to-be 1)))
  (it "doesn't find non-existent entry"
    (expect (purpose-alist-get 'c '((a . 1) (b . 2))) :to-be nil))
  (it "returns default when key not found"
    (expect (purpose-alist-get 'c '((a . 1) (b . 2)) 5) :to-be 5)))

(describe "purpose-alist-set"
  (it "sets existing entry"
    (expect (purpose-alist-set 'b 3 '((a . 1) (b . 2))) :to-equal '((b . 3) (a . 1))))
  (it "sets non-existent entry"
    (expect (purpose-alist-set 'c 3 '((a . 1) (b . 2))) :to-equal '((c . 3) (a . 1) (b . 2)))))

(describe "purpose-alist-del"
  (it "removes all instances of unwanted key"
    (expect (purpose-alist-del 'a '((a . 1) (b . 2) (a . 3))) :to-equal '((b . 2))))
  (it "does nothing when unwanted key doesn't exist'"
    (expect (purpose-alist-del 'c '((a . 1) (b . 2))) :to-equal '((a . 1) (b . 2)))))

(describe "purpose-flatten"
  (it "flattens a nested list"
    (expect (purpose-flatten '((1 2) (3 4))) :to-equal '(1 2 3 4)))
  (it "doesn't flatten more than one level of nesting"
    (expect (purpose-flatten '(((1 2) 3 4) (5))) :to-equal '((1 2) 3 4 5))))

(describe "purpose-alist-combine"
  (it "removes duplicates and prefers first value for duplicated keys"
    (expect (purpose-alist-combine '((a . 1) (b . 2)) '((a . 3) (c . 4)))
            :to-have-same-items-as '((a . 1) (b . 2) (c . 4)))))

(describe "purpose-plist-value"
  (it "returns only and all the values"
    (expect (purpose-plist-values '(:a 1 :b 2 :c 3)) :to-equal '(1 2 3))))

;; ignore advice-compatiblity utils, because we're gonna get rid of them
;; (describe "purpose-advice-convert-where-arg")
;; (describe "purpose-advice-new-style-arglist")
;; (describe "define-purpose-compatible-advice")
;; (describe "purpose-advice-add")
;; (describe "purpose-advice-remove")

(describe "purpose-hash-table-values"
  (it "returns only and all the values"
    (let ((myhash (make-hash-table)))
      (puthash 'a 1 myhash)
      (puthash 'b 2 myhash)
      (puthash 'c 3 myhash)
      (expect (purpose-hash-table-values myhash) :to-have-same-items-as '(1 2 3)))))

(describe "purpose--suffix-p"
  (it "returns non-nil when string ends with suffix"
    (expect (purpose--suffix-p ".good" "not.good") :to-be-truthy))
  (it "returns nil when string doesn't contain suffix"
    (expect (purpose--suffix-p ".go" "not.good") :not :to-be-truthy))
  (it "returns nil when string contains suffix but not at the end"
    (expect (purpose--suffix-p "bad" "not.good") :not :to-be-truthy)))

(describe "purpose--remove-suffix"
  (it "returns shortened string when string ends with suffix"
    (expect (purpose--remove-suffix ".good" "not.good") :to-equal "not"))
  (it "returns original string when string doesn't contain suffix"
    (expect (purpose--remove-suffix ".go" "not.good") :to-equal "not.good"))
  (it "returns original string when string contains suffix but not at the end"
    (expect (purpose--remove-suffix "bad" "not.good") :to-equal "not.good")))
