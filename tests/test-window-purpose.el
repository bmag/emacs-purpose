;; -*- lexical-binding: t -*-

;; based on https://github.com/sviridov/undercover.el-buttercup-integration-example/blob/master/tests/test-multiply.el
(require 'undercover-init.el)
(require 'window-purpose)

(describe "purpose--modeline-string"
  :var (config-snapshot)
  (before-all
    (setq config-snapshot (get-purpose-config))
    (load-purpose-config
     (make-purpose-config :names '(("xxx-test" . edit))))
    (create-buffers "xxx-test"))
  (after-all
    (load-purpose-config config-snapshot))

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
