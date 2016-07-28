;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(defvar frame-layout-testfiles
  '(("tests/layouts1/test-edit-terminal.frame-layout" .
     ((nil
       (0 0 232 61)
       (t
        (0 0 116 61)
        (:purpose edit :purpose-dedicated t :width 0.5065502183406113 :height 0.4838709677419355 :edges
                  (0.0 0.0 0.5065502183406113 0.4838709677419355))
        (:purpose edit :purpose-dedicated nil :width 0.5065502183406113 :height 0.5 :edges
                  (0.0 0.4838709677419355 0.5065502183406113 0.9838709677419355)))
       (t
        (116 0 232 61)
        (:purpose terminal :purpose-dedicated nil :width 0.5065502183406113 :height 0.4838709677419355 :edges
                  (0.5065502183406113 0.0 1.0131004366812226 0.4838709677419355))
        (:purpose terminal :purpose-dedicated nil :width 0.5065502183406113 :height 0.5 :edges
                  (0.5065502183406113 0.4838709677419355 1.0131004366812226 0.9838709677419355))))))
    ("tests/layouts1/test-extra.frame-layout" .
     (nil
      (0 0 240 56)
      (:purpose edit :purpose-dedicated nil :width 0.5042016806722689 :height 0.9824561403508771 :edges
                (0.0 0.0 0.5042016806722689 0.9824561403508771))
      (t
       (120 0 240 56)
       (:purpose edit :purpose-dedicated nil :width 0.5042016806722689 :height 0.49122807017543857 :edges
                 (0.5042016806722689 0.0 1.0084033613445378 0.49122807017543857))
       (:purpose dired :purpose-dedicated nil :width 0.5042016806722689 :height 0.49122807017543857 :edges
                 (0.5042016806722689 0.49122807017543857 1.0084033613445378 0.9824561403508771)))))))

(defvar window-layout-testfiles
  '(("tests/layouts1/test-dired2.window-layout" .
     (nil
      (0 0 232 61)
      (:purpose dired :purpose-dedicated nil :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.0 0.0 0.5065502183406113 0.9838709677419355))
      (:purpose dired :purpose-dedicated nil :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.5065502183406113 0.0 1.0131004366812226 0.9838709677419355))))
    ("tests/layouts1/test-edit-terminal.window-layout" .
     (nil
      (0 0 232 61)
      (t
       (0 0 116 61)
       (:purpose edit :purpose-dedicated t :width 0.5065502183406113 :height 0.4838709677419355 :edges
                 (0.0 0.0 0.5065502183406113 0.4838709677419355))
       (:purpose edit :purpose-dedicated nil :width 0.5065502183406113 :height 0.5 :edges
                 (0.0 0.4838709677419355 0.5065502183406113 0.9838709677419355)))
      (:purpose terminal :purpose-dedicated nil :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.5065502183406113 0.0 1.0131004366812226 0.9838709677419355))))
    ("tests/layouts2/test-dired-edit-general.window-layout" .
     (nil
      (0 0 232 61)
      (:purpose dired :purpose-dedicated t :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.0 0.0 0.5065502183406113 0.9838709677419355))
      (t
       (116 0 232 61)
       (:purpose edit :purpose-dedicated nil :width 0.5065502183406113 :height 0.4838709677419355 :edges
                 (0.5065502183406113 0.0 1.0131004366812226 0.4838709677419355))
       (:purpose general :purpose-dedicated nil :width 0.5065502183406113 :height 0.5 :edges
                 (0.5065502183406113 0.4838709677419355 1.0131004366812226 0.9838709677419355)))))
    ("tests/layouts2/test-edit-terminal.window-layout" .
     (nil
      (0 0 232 61)
      (:purpose edit :purpose-dedicated t :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.0 0.0 0.5065502183406113 0.9838709677419355))
      (:purpose terminal :purpose-dedicated nil :width 0.5065502183406113 :height 0.9838709677419355 :edges
                (0.5065502183406113 0.0 1.0131004366812226 0.9838709677419355))))
    ("tests/layouts2/test-extra.frame-layout" .
     (nil
      (0 0 240 56)
      (:purpose edit :purpose-dedicated nil :width 0.5042016806722689 :height 0.9824561403508771 :edges
                (0.0 0.0 0.5042016806722689 0.9824561403508771))
      (t
       (120 0 240 56)
       (:purpose edit :purpose-dedicated nil :width 0.5042016806722689 :height 0.49122807017543857 :edges
                 (0.5042016806722689 0.0 1.0084033613445378 0.49122807017543857))
       (:purpose dired :purpose-dedicated nil :width 0.5042016806722689 :height 0.49122807017543857 :edges
                 (0.5042016806722689 0.49122807017543857 1.0084033613445378 0.9824561403508771)))))
    ))

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

  (describe "purpose-window-params" (it "returns a window-params object"
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

  (describe "Windows"
    (describe "purpose-all-window-layouts"
      (it "finds all layouts (and sorts them)"
        (expect (purpose-all-window-layouts)
                :to-equal
                '("test-dired-edit-general" "test-dired2" "test-edit-terminal")))
      (it "returns nil when there are no layouts"
        (expect (purpose-all-window-layouts '("some/non-existent/dir")) :to-be nil)))

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
          (expect (purpose-window-purpose-dedicated-p win3) :not :to-be-truthy))))

    (describe "purpose-save-window-layout-file"
      :var (testfile)
      (before-all
        (setq testfile "testlayout.window-layout"))
      (before-each
        (when (file-exists-p testfile)
          (delete-file testfile)))
      (it "creates a file"
        (purpose-save-window-layout-file testfile)
        (expect testfile :to-exist))
      (it "saves the current layout to the file"
        (purpose-save-window-layout-file testfile)
        (expect testfile :to-exist)
        (let ((contents (with-temp-buffer
                          (insert-file-contents-literally testfile)
                          (read (current-buffer)))))
          (expect contents :to-equal (purpose-get-window-layout)))))

    (describe "purpose-load-window-layout-file"
      :var (testfile expected-layout)
      (before-all
        (let ((entry (assoc "tests/layouts1/test-dired2.window-layout" window-layout-testfiles)))
          (expect entry)
          (setq testfile (car entry)
                expected-layout (cdr entry))))

      (it "loads correct layout from file"
        (spy-on 'purpose-set-window-layout)
        (purpose-load-window-layout-file testfile)
        (expect 'purpose-set-window-layout :to-have-been-called-with expected-layout))
      (it "throws error when file doesn't exist"
        (spy-on 'purpose-set-window-layout)
        (expect (lambda () (purpose-load-window-layout-file "this-file-does-not-exist"))
                :to-throw 'error)))

    (describe "purpose-save-window-layout"
      :var (layout-name layout-dir new-dir)
      (before-all
        (setq layout-dir (car purpose-layout-dirs)
              layout-name "test-dired2"
              new-dir "new-directory")
        (when (file-exists-p new-dir)
          (delete-directory new-dir t)))
      (after-all
        (when (file-exists-p new-dir)
          (delete-directory new-dir t)))
      (it "saves to correct file"
        (spy-on 'purpose-save-window-layout-file)
        (purpose-save-window-layout layout-name layout-dir)
        (expect 'purpose-save-window-layout-file :to-have-been-called-with
                (concat (file-name-as-directory layout-dir) layout-name ".window-layout")))
      (it "creates directory when needed"
        (spy-on 'purpose-save-window-layout-file)
        (purpose-save-window-layout layout-name new-dir)
        (expect 'purpose-save-window-layout-file :to-have-been-called-with
                (concat (file-name-as-directory new-dir) layout-name ".window-layout"))
        (expect new-dir :to-exist)))

    (describe "purpose-load-window-layout"
      (it "loads correct file from directory"
        (spy-on 'purpose-load-window-layout-file)
        (purpose-load-window-layout "test-dired2")
        (expect 'purpose-load-window-layout-file :to-have-been-called-with
                (expand-file-name "tests/layouts1/test-dired2.window-layout")))))

  (describe "Frames"
    (describe "purpose-all-frame-layouts"
      (it "finds all layouts (and sorts them)"
        (expect (purpose-all-frame-layouts)
                :to-equal
                '("test-edit-terminal" "test-extra")))
      (it "returns nil when there are no layouts"
        (expect (purpose-all-frame-layouts '("some/non-existent/dir")) :to-be nil)))

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

    (describe "purpose-get-frame-layout"
      :var (call-count)
      (before-each
        (setq call-count 0))
      (it "calls `purpose-get-window-layout' once for each frame"
        (spy-on 'purpose-get-window-layout :and-call-fake (lambda (x) (cl-incf call-count)))
        (expect (purpose-get-frame-layout) :to-equal '(1))
        (expect call-count :to-be 1)
        (spy-on 'frame-list :and-return-value '(a b))
        (expect (purpose-get-frame-layout) :to-equal '(2 3))
        (expect call-count :to-be 3)))

    (describe "purpose-set-frame-layout"
      :var (call-count status)
      (before-each
        (setq call-count 0)
        (setq status nil)
        (spy-on 'make-frame)
        (spy-on 'purpose-set-window-layout :and-call-fake (lambda (&rest _args) (cl-incf call-count)))
        (spy-on 'delete-other-frames :and-call-fake (lambda (&rest _args)
                                                      (setq status (if (= call-count 0) 'before 'after))))
        (purpose-set-frame-layout '(a b)))
      (it "deletes all frames first"
        (expect status :to-be 'before))
      (it "calls `purpose-set-window-layout' once for each frame"
        (expect call-count :to-be 2)))

    (describe "purpose-save-frame-layout-file"
      :var (testfile)
      (before-all
        (setq testfile "testlayout.frame-layout"))
      (before-each
        (when (file-exists-p testfile)
          (delete-file testfile)))
      (it "creates a file"
        (purpose-save-frame-layout-file testfile)
        (expect testfile :to-exist))
      (it "saves the current layout to the file"
        (purpose-save-frame-layout-file testfile)
        (expect testfile :to-exist)
        (let ((contents (with-temp-buffer
                          (insert-file-contents-literally testfile)
                          (read (current-buffer)))))
          (expect contents :to-equal (purpose-get-frame-layout)))))

    (describe "purpose-load-frame-layout-file"
      :var (testfile expected-layout)
      (before-all
        (let ((entry (assoc "tests/layouts1/test-edit-terminal.frame-layout" frame-layout-testfiles)))
          (expect entry)
          (setq testfile (car entry)
                expected-layout (cdr entry))))

      (it "loads correct layout from file"
        (spy-on 'purpose-set-frame-layout)
        (purpose-load-frame-layout-file testfile)
        (expect 'purpose-set-frame-layout :to-have-been-called-with expected-layout))
      (it "throws error when file doesn't exist"
        (spy-on 'purpose-set-frame-layout)
        (expect (lambda () (purpose-load-frame-layout-file "this-file-does-not-exist"))
                :to-throw 'error)))

    (describe "purpose-save-frame-layout"
      :var (layout-name layout-dir new-dir)
      (before-all
        (setq layout-dir (car purpose-layout-dirs)
              layout-name "test-edit-terminal"
              new-dir "new-directory")
        (when (file-exists-p new-dir)
          (delete-directory new-dir t)))
      (after-all
        (when (file-exists-p new-dir)
          (delete-directory new-dir t)))
      (it "saves to correct file"
        (spy-on 'purpose-save-frame-layout-file)
        (purpose-save-frame-layout layout-name layout-dir)
        (expect 'purpose-save-frame-layout-file :to-have-been-called-with
                (concat (file-name-as-directory layout-dir) layout-name ".frame-layout")))
      (it "creates directory when needed"
        (spy-on 'purpose-save-frame-layout-file)
        (purpose-save-frame-layout layout-name new-dir)
        (expect 'purpose-save-frame-layout-file :to-have-been-called-with
                (concat (file-name-as-directory new-dir) layout-name ".frame-layout"))
        (expect new-dir :to-exist)))
    (describe "purpose-load-frame-layout"
      (it "loads correct file from directory"
        (spy-on 'purpose-load-frame-layout-file)
        (purpose-load-frame-layout "test-edit-terminal")
        (expect 'purpose-load-frame-layout-file :to-have-been-called-with
                (expand-file-name "tests/layouts1/test-edit-terminal.frame-layout")))))

  (describe "Recent/Reset Layouts"
    :var (recent-window-layouts-backup recent-frame-layouts-backup
                                       wlayout1 wlayout2
                                       flayout1 flayout2)
    (before-all
      (setq recent-window-layouts-backup purpose-recent-window-layouts
            recent-frame-layouts-backup purpose-recent-frame-layouts)
      (setq wlayout1 (cdr (nth 0 window-layout-testfiles))
            wlayout2 (cdr (nth 1 window-layout-testfiles)))
      (setq flayout1 (cdr (nth 0 frame-layout-testfiles))
            flayout2 (cdr (nth 1 frame-layout-testfiles))))
    (after-all
      (setq purpose-recent-window-layouts recent-window-layouts-backup
            purpose-recent-frame-layouts recent-frame-layouts-backup))

    (describe "purpose-reset-window-layout"
      (it "loads the recent layout if there is one"
        (setq purpose-recent-window-layouts (make-ring 50))
        (ring-insert purpose-recent-window-layouts 'a)
        (spy-on 'purpose-load-recent-window-layout)
        (purpose-reset-window-layout)
        (expect 'purpose-load-recent-window-layout :to-have-been-called-with 0))
      (it "does nothing when there is no recent layout"
        (setq purpose-recent-window-layouts (make-ring 50))
        (spy-on 'purpose-load-recent-window-layout)
        (expect (purpose-reset-window-layout) :to-be nil)
        (expect 'purpose-load-recent-window-layout :not :to-have-been-called)))

    (describe "purpose-load-recent-window-layout"
      (it "loads correct items"
        (setq purpose-recent-window-layouts (make-ring 50))
        (ring-insert purpose-recent-window-layouts 'b)
        (ring-insert purpose-recent-window-layouts 'a)
        (spy-on 'purpose-set-window-layout)
        (purpose-load-recent-window-layout 0)
        (expect 'purpose-set-window-layout :to-have-been-called-with 'a nil t)
        (purpose-load-recent-window-layout 1)
        (expect 'purpose-set-window-layout :to-have-been-called-with 'b nil nil)))

    (describe "purpose-reset-frame-layout"
      (it "loads the recent layout if there is one"
        (setq purpose-recent-frame-layouts (make-ring 50))
        (ring-insert purpose-recent-frame-layouts 'a)
        (spy-on 'purpose-load-recent-frame-layout)
        (purpose-reset-frame-layout)
        (expect 'purpose-load-recent-frame-layout :to-have-been-called-with 0))
      (it "does nothing when there is no recent layout"
        (setq purpose-recent-frame-layouts (make-ring 50))
        (spy-on 'purpose-load-recent-frame-layout)
        (expect (purpose-reset-frame-layout) :to-be nil)
        (expect 'purpose-load-recent-frame-layout :not :to-have-been-called)))

    (describe "purpose-load-recent-frame-layout"
      (it "loads correct items"
        (setq purpose-recent-frame-layouts (make-ring 50))
        (ring-insert purpose-recent-frame-layouts 'b)
        (ring-insert purpose-recent-frame-layouts 'a)
        (spy-on 'purpose-set-frame-layout)
        (purpose-load-recent-frame-layout 0)
        (expect 'purpose-set-frame-layout :to-have-been-called-with 'a t)
        (purpose-load-recent-frame-layout 1)
        (expect 'purpose-set-frame-layout :to-have-been-called-with 'b nil))))

  ;; TODO: `purpose-set-window-layout' should change `purpose-recent-window-layouts' (unless norecord)
  ;; TODO: test all interactive calls interactively
  ;; TODO: `purpose-delete-window-at-*' functions
  )
