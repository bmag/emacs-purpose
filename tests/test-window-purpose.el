;; -*- lexical-binding: t -*-

;; based on https://github.com/sviridov/undercover.el-buttercup-integration-example/blob/master/tests/test-multiply.el
(require 'undercover-init.el)
(require 'window-purpose)

(describe "A suite"
  (it "contains a spec with an expectation"
      (expect t :to-be t)))
