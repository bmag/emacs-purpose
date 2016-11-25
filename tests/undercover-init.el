;; from https://github.com/sviridov/undercover.el-buttercup-integration-example/blob/master/tests/undercover-init.el
(when (require 'undercover nil t)
  (undercover "*.el"))

(provide 'undercover-init.el)
