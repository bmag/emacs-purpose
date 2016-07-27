;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "Layout Suite"
  :var (config-suite-snapshot config-case-snapshot original-layout-dirs)
  (before-all
    (purpose-mode)
    (setq config-suite-snapshot (get-purpose-config))
    (setq original-layout-dirs purpose-layout-dirs)
    (setq purpose-layout-dirs '("tests/layouts2" "tests/layouts1")))

  (after-all
    (setq purpose-layout-dirs original-layout-dirs)
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

  (describe "purpose-normalize-layout-directories"
    (it "defaults to only purpose-layout-dirs"
      (expect (purpose-normalize-layout-directories) :to-equal purpose-layout-dirs))
    (it "adds built-in layout dir when asked"
      (expect (purpose-normalize-layout-directories (list "a") t)
              :to-equal (list "a" purpose--built-in-layouts-dir)))
    (it "uses other dirs when asked"
      (expect (purpose-normalize-layout-directories '("a" "b")) :to-equal '("a" "b"))))

  (describe "purpose-all-window-layouts"
    (it "finds all layouts (and sorts them)"
      (expect (purpose-all-window-layouts)
              :to-equal
              '("test-dired-edit-general" "test-dired2" "test-edit-terminal")))
    (it "returns nil when there are no layouts"
      (expect (purpose-all-window-layouts '("some/non-existent/dir")) :to-be nil)))

  (describe "purpose-all-frame-layouts"
    (it "finds all layouts (and sorts them)"
      (expect (purpose-all-frame-layouts)
              :to-equal
              '("test-edit-terminal" "test-extra")))
    (it "returns nil when there are no layouts"
      (expect (purpose-all-frame-layouts '("some/non-existent/dir")) :to-be nil)))

  (describe "purpose-find-window-layout"
    (it "returns filename for layout"
      (expect (purpose-find-window-layout "test-dired2") :to-equal
              (expand-file-name "tests/layouts1/test-dired2.window-layout")))
    (it "returns filename for higher priority layout when there are several"
      ;; layouts2 is before layouts1 in `purpose-layout-dirs'
      (expect (purpose-find-window-layout "test-edit-terminal") :to-equal
              (expand-file-name "tests/layouts2/test-edit-terminal.window-layout")))
    (it "returns nil for non-existent layout"
      (expect (purpose-find-window-layout "non-existent-layout") :to-be nil)))

  (describe "purpose-find-frame-layout"
    (it "returns filename for layout"
      (expect (purpose-find-frame-layout "test-edit-terminal") :to-equal
              (expand-file-name "tests/layouts1/test-edit-terminal.frame-layout")))
    (it "returns filename for higher priority layout when there are several"
      ;; layouts2 is before layouts1 in `purpose-layout-dirs'
      (expect (purpose-find-frame-layout "test-extra") :to-equal
              (expand-file-name "tests/layouts2/test-extra.frame-layout")))
    (it "returns nil for non-existent layout"
      (expect (purpose-find-frame-layout "non-existent-layout") :to-be nil)))

  (describe "purpose-get-window-layout"
    :var (result)
    (before-each
      (setq win2 (split-window nil nil 'below))
      (purpose-set-window-purpose 'foo1 'dont-dedicate)
      (with-selected-window win2
        (purpose-set-window-purpose 'foo2))
      (setq result (purpose-get-window-layout)))
    (it "reports correct splits"
      (expect (car result) :to-be t)
      (expect (length result) :to-be 4)
      (expect (purpose-window-params-p (nth 2 result)) :to-be-truthy)
      (expect (purpose-window-params-p (nth 3 result)) :to-be-truthy))
    (it "reports correct sizes"
      (let ((win1 (nth 2 result))
            (win2 (nth 3 result)))
        (expect (plist-get win1 :width) :to-be-close-to 1.0 1)
        (expect (plist-get win1 :height) :to-be-close-to 0.5 1)
        (expect (plist-get win2 :width) :to-be-close-to 1.0 1)
        (expect (plist-get win2 :height) :to-be-close-to 0.5 1)))
    (it "reports correct purposes"
      (let ((win1 (nth 2 result))
            (win2 (nth 3 result)))
        (expect (plist-get win1 :purpose) :to-be 'foo1)
        (expect (plist-get win2 :purpose) :to-be 'foo2)))
    (it "reports correct dedication status"
      (let ((win1 (nth 2 result))
            (win2 (nth 3 result)))
        (expect (plist-get win1 :purpose-dedicated) :not :to-be-truthy)
        (expect (plist-get win2 :purpose-dedicated) :to-be-truthy))))

  (describe "purpose-set-window-layout"
    :var (layout result real-tree)
    (before-all
      ;; layout:
      ;; +----------+----------+
      ;; |          |  foo2    |
      ;; |          |          |
      ;; |  foo1    |----------|
      ;; |          |  foo3    |
      ;; |          |          |
      ;; +----------+----------+
      (setq layout
            '(nil
              (0 0 232 61)
              (:purpose foo1 :purpose-dedicated t :width 0.25 :height 1.0 :edges (0.0 0.0 0.25 1.0))
              (t
               (58 0 232 61)
               (:purpose foo2 :purpose-dedicated nil :width 0.75 :height 0.6 :edges (0.25 0.0 1.0 0.6))
               (:purpose foo3 :purpose-dedicated nil :width 0.75 :height 0.4 :edges (0.25 0.6 1.0 1.0))))))
    (before-each
      (purpose-set-window-layout layout)
      (setq result (purpose-get-window-layout))
      (setq real-tree (car (window-tree))))
    (it "creates correct splits"
      (expect (car result) :to-be nil)
      (expect (length result) :to-be 4)
      (let ((win1 (nth 2 result))
            (tree2 (nth 3 result)))
        (expect (purpose-window-params-p win1) :to-be-truthy)
        (expect (car tree2) :to-be t)
        (expect (length tree2) :to-be 4)
        (expect (purpose-window-params-p (nth 2 tree2)) :to-be-truthy)
        (expect (purpose-window-params-p (nth 3 tree2)) :to-be-truthy)))
    (it "creates correct sizes"
      (let* ((win1 (nth 2 result))
             (tree2 (nth 3 result))
             (win2 (nth 2 tree2))
             (win3 (nth 3 tree2)))
        (expect (plist-get win1 :width) :to-be-close-to 0.25 1)
        (expect (plist-get win1 :height) :to-be-close-to 1.0 1)
        (expect (plist-get win2 :width) :to-be-close-to 0.75 1)
        (expect (plist-get win2 :height) :to-be-close-to 0.5 1)
        (expect (plist-get win3 :width) :to-be-close-to 0.75 1)
        (expect (plist-get win3 :height) :to-be-close-to 0.4 1)))
    (it "creates correct purposes"
      (let* ((win1 (nth 2 real-tree))
             (tree2 (nth 3 real-tree))
             (win2 (nth 2 tree2))
             (win3 (nth 3 tree2)))
        (expect (purpose-window-purpose win1) :to-be 'foo1)
        (expect (purpose-window-purpose win2) :to-be 'foo2)
        (expect (purpose-window-purpose win3) :to-be 'foo3)))
    (it "creates correct dedication status"
      (let* ((win1 (nth 2 real-tree))
             (tree2 (nth 3 real-tree))
             (win2 (nth 2 tree2))
             (win3 (nth 3 tree2)))
        (expect (purpose-window-purpose-dedicated-p win1) :to-be-truthy)
        (expect (purpose-window-purpose-dedicated-p win2) :not :to-be-truthy)
        (expect (purpose-window-purpose-dedicated-p win3) :not :to-be-truthy)))
    )
  )
