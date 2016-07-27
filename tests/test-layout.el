;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "Layout Suite"
  :var (config-suite-snapshot config-case-snapshot)
  (before-all
    (purpose-mode)
    (setq config-suite-snapshot (get-purpose-config)))

  (after-all
    (load-purpose-config config-suite-snapshot)
    (purpose-mode -1))

  (before-each
    (setq config-case-snapshot (get-purpose-config))
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0")
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (set-window-dedicated-p nil nil)
    (purpose-set-window-purpose-dedicated-p nil nil)
    (set-window-buffer nil "xxx-p0-0"))

  (after-each
    (load-purpose-config config-suite-snapshot))

  (describe "purpose-window-params"
    (it "returns a window-params object"
      (load-purpose-config (make-purpose-config :names '(("xxx-p0-0" . p0))))
      (set-window-buffer nil "xxx-p0-0")
      (purpose-set-window-purpose-dedicated-p nil t)
      (let* ((obj (purpose-window-params))
             (edges (plist-get obj :edges)))
        (expect (plist-get obj :purpose) :to-be 'p0)
        (expect (plist-get obj :purpose-dedicated) :to-be t)
        (expect (plist-get obj :width) :to-be-close-to 1.0 1)
        (expect (plist-get obj :height) :to-be-close-to 1.0 1)
        (expect (nth 0 edges) :to-be-close-to 0.0 1)
        (expect (nth 1 edges) :to-be-close-to 0.0 1)
        (expect (nth 2 edges) :to-be-close-to 1.0 1)
        (expect (nth 3 edges) :to-be-close-to 1.0 1))))

  (describe "purpose-window-params-p"
    (it "recognizes plist with a :purpose key"
      (expect (purpose-window-params-p '(:purpose p0)) :to-be-truthy))
    (it "doesn't recognize plist without a :purpose key"
      (expect (purpose-window-params-p '(:purpose-dedicated t)) :not :to-be-truthy))
    (it "doesn't recognize non-lists"
      (expect (purpose-window-params-p 1) :not :to-be-truthy)))

  (describe "purpose-set-window-properties"
    (before-each
      (load-purpose-config (make-purpose-config :names '(("xxx-p0-0" . p0)))))
    (it "sets :purpose correctly"
      (purpose-set-window-properties '(:purpose p0))
      (expect '(:purpose p0) :to-match-window-tree))
    (it "sets :purpose-dedicated to nil correctly"
      (purpose-set-window-properties '(:purpose p0 :purpose-dedicated nil))
      (expect '(:purpose p0 :p-ded nil) :to-match-window-tree))
    (it "sets :purpose-dedicated to t correctly"
      (purpose-set-window-properties '(:purpose p0 :purpose-dedicated t))
      (expect '(:purpose p0 :p-ded t) :to-match-window-tree)))

  (describe "purpose-set-window-purpose"
    (it "should change window's purpose"
      (purpose-set-window-purpose 'foo)
      (expect (purpose-window-purpose) :to-be 'foo)))

  (describe "purpose-delete-non-dedicated-windows"
    (it "should delete only and all non-dedicated windows"
      (let ((win1 nil) win2 win3 win4)
        (setq win2 (split-window win1 nil 'below))
        (setq win3 (split-window win1 nil 'right))
        (setq win4 (split-window win2 nil 'right))
        ;; frame:
        ;; +---------+---------+
        ;; |   win1  |  win3   |
        ;; +---------+---------+
        ;; |   win2  |  win4   |
        ;; +---------+---------+
        (purpose-set-window-properties '(:purpose foo1 :purpose-dedicated nil) win1)
        (purpose-set-window-properties '(:purpose foo2 :purpose-dedicated t) win2)
        (purpose-set-window-properties '(:purpose foo3 :purpose-dedicated t) win3)
        (purpose-set-window-properties '(:purpose foo4 :purpose-dedicated nil) win4)
        (expect '(split (split (:purpose foo1) (:purpose foo3))
                        (split (:purpose foo2) (:purpose foo4)))
                :to-match-window-tree)
        (purpose-delete-non-dedicated-windows)
        (expect '(split (:purpose foo3 :p-ded t) (:purpose foo2 :p-ded t))
                :to-match-window-tree))))
  )
