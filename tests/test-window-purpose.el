;; -*- lexical-binding: t -*-

(require 'buttercup-init)

(describe "purpose--modeline-string"
  :var (config-snapshot)
  (before-all
    (setq config-snapshot (get-purpose-config-2))
    (load-purpose-config-2
     (make-purpose-config-2  '((:origin test :priority 70 :purpose edit :name "xxx-test"))))
    (create-buffers "xxx-test"))
  (after-all
    (load-purpose-config-2 config-snapshot))

  (it "shows buffer's purpose"
    (build-one-window '(:name "xxx-test" :b-ded nil :p-ded nil))
    (expect (purpose--modeline-string) :to-equal " [edit]"))
  (it "marks buffer dedication with \"#\""
    (build-one-window '(:name "xxx-test" :b-ded t :p-ded nil))
    (expect (purpose--modeline-string) :to-equal " [edit#]"))
  (it "marks purpose dedication with \"!\""
    (build-one-window '(:name "xxx-test" :b-ded nil :p-ded t))
    (expect (purpose--modeline-string) :to-equal " [edit!]"))
  (it "marks purpose dedication before buffer dedication"
    (build-one-window '(:name "xxx-test" :b-ded t :p-ded t))
    (expect (purpose--modeline-string) :to-equal " [edit!#]")))
