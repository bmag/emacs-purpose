;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "switch-buffer suite"
  :var (config-snapshot)
  (before-all
    (purpose-mode)
    (setq config-snapshot (get-purpose-config))
    (load-purpose-config
     (make-purpose-config :regexps '(("^xxx-p0-" . p0)
                                     ("^xxx-p1-" . p1)))))
  (after-all
    (load-purpose-config config-snapshot)
    (purpose-mode -1))
  (before-each
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0")
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (set-window-dedicated-p nil nil)
    (purpose-set-window-purpose-dedicated-p nil nil)
    (set-window-buffer nil "xxx-p0-0"))

  (it "switch-to-buffer to same purpose"
    (switch-to-buffer "xxx-p0-1")
    ;; (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-1"))
    (expect '(:name "xxx-p0-1") :to-match-window-tree)
    )

  (it "switch-to-buffer to other purpose"
    (switch-to-buffer "xxx-p1-0")
    ;; (expect (selected-frame) :to-show-exactly-buffers '("xxx-p1-0"))
    (expect (extract-window-tree) :to-match-window-recipe '(:name "xxx-p1-0")))

  (it "switch-to-buffer from purpose-dedicated to same purpose"
    (purpose-set-window-purpose-dedicated-p nil t)
    (switch-to-buffer "xxx-p0-1")
    ;; (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-1"))
    ;; (expect (purpose-window-purpose-dedicated-p))
    (expect '(:name "xxx-p0-1" :p-ded t) :to-match-window-tree)
    )

  (it "switch-to-buffer from purpose-dedicated to other purpose"
    (purpose-set-window-purpose-dedicated-p nil t)
    (switch-to-buffer "xxx-p1-0")
    ;; (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-0" "xxx-p1-0"))
    ;; (expect (selected-window) :to-show-buffer "xxx-p1-0")
    ;; (expect (next-window) :to-show-buffer "xxx-p0-0")
    ;; (expect (purpose-window-purpose-dedicated-p) :to-be nil)
    ;; (expect (purpose-window-purpose-dedicated-p (next-window)) :to-be t)
    (expect '(split (:name "xxx-p0-0" :p-ded t)
                    (:name "xxx-p1-0" :p-ded nil :selected t))
            :to-match-window-tree))
  )
