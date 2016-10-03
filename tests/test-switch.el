;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(describe "switch-to-buffer"
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
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))

  (it "with 1 window, switching to same purpose reuses window"
    (build-one-window '(:name "xxx-p0-0"))
    (switch-to-buffer "xxx-p0-1")
    (expect '(:name "xxx-p0-1") :to-match-window-tree))

  (it "with 2 windows, switching to other purpose reuses other window"
    (build-two-windows '((:name "xxx-p1-0" :selected t) (:name "xxx-p0-0")))
    (switch-to-buffer "xxx-p0-1")
    (expect '(split (:name "xxx-p1-0")
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))

  (it "with 1 window, switching to other purpose reuses window"
    (build-one-window '(:name "xxx-p0-0"))
    (switch-to-buffer "xxx-p1-0")
    (expect '(:name "xxx-p1-0") :to-match-window-tree))

  (it "with 1 purpose-dedicated window, switching to same purpose reuses window"
    (build-one-window '(:name "xxx-p0-0" :p-ded t))
    (switch-to-buffer "xxx-p0-1")
    (expect '(:name "xxx-p0-1" :p-ded t) :to-match-window-tree))

  (it "with 1 buffer-dedicated window, switching to same purpose creates new window"
    (build-one-window '(:name "xxx-p0-0" :b-ded t))
    (switch-to-buffer "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0" :b-ded t)
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))

  (it "with 1 purpose-dedicated window, switching to other purpose creates new window"
    (build-one-window '(:name "xxx-p0-0" :p-ded t))
    (switch-to-buffer "xxx-p1-0")
    (expect '(split (:name "xxx-p0-0" :p-ded t)
                    (:name "xxx-p1-0" :p-ded nil :selected t))
            :to-match-window-tree)))

(describe "switch-to-buffer-other-window"
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
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))

  (it "with 1 window, switching to same purpose creates new window"
    (build-one-window '(:name "xxx-p0-0"))
    (switch-to-buffer-other-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0")
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))
  (it "with 2 windows, switching to same purpose uses other window"
    (build-two-windows '((:name "xxx-p0-0" :selected t)
                         (:name "xxx-p1-0")))
    (switch-to-buffer-other-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0")
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))
  (it "with 2 windows, other window purpose-dedicated, swithing to same purpose creates new window"
    (build-two-windows '((:name "xxx-p0-0" :selected t)
                         (:name "xxx-p1-0" :p-ded t)))
    (switch-to-buffer-other-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0")
                    (:name "xxx-p0-1" :selected t)
                    (:name "xxx-p1-0" :p-ded t))
            :to-match-window-tree))
  (it "with 2 windows, other window buffer-dedicated, swithing to same purpose creates new window"
    (build-two-windows '((:name "xxx-p0-0" :selected t)
                         (:name "xxx-p1-0" :p-ded t)))
    (switch-to-buffer-other-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0")
                    (:name "xxx-p0-1" :selected t)
                    (:name "xxx-p1-0" :p-ded t))
            :to-match-window-tree)))

(describe "pop-to-buffer"
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
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))

  (it "with 1 window, switching to same purpose reuses window"
    (build-one-window '(:name "xxx-p0-0"))
    (pop-to-buffer "xxx-p0-1")
    (expect '(:name "xxx-p0-1") :to-match-window-tree))

  (it "with 2 windows, switching to same purpose reuses window"
    (build-two-windows '((:name "xxx-p0-0" :selected t) (:name "xxx-p1-0")))
    (pop-to-buffer "xxx-p0-1")
    (expect '(split (:name "xxx-p0-1" :selected t)
                    (:name "xxx-p1-0"))
            :to-match-window-tree))

  (it "with 2 windows, switching to other purpose reuses other window"
    (build-two-windows '((:name "xxx-p1-0" :selected t) (:name "xxx-p0-0")))
    (pop-to-buffer "xxx-p0-1")
    (expect '(split (:name "xxx-p1-0")
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))

  (it "with 1 window, switching to other purpose creates new window"
    (build-one-window '(:name "xxx-p0-0"))
    (pop-to-buffer "xxx-p1-0")
    (expect '(split (:name "xxx-p0-0")
                    (:name "xxx-p1-0" :selected t))
            :to-match-window-tree))

  (it "with 1 purpose-dedicated window, switching to same purpose reuses window"
    (build-one-window '(:name "xxx-p0-0" :p-ded t))
    (pop-to-buffer "xxx-p0-1")
    (expect '(:name "xxx-p0-1" :p-ded t) :to-match-window-tree))

  (it "with 1 buffer-dedicated window, switching to same purpose creates new window"
    (build-one-window '(:name "xxx-p0-0" :b-ded t))
    (pop-to-buffer "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0" :b-ded t)
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))

  (it "with 1 purpose-dedicated window, switching to other purpose creates new window"
    (build-one-window '(:name "xxx-p0-0" :p-ded t))
    (pop-to-buffer "xxx-p1-0")
    (expect '(split (:name "xxx-p0-0" :p-ded t)
                    (:name "xxx-p1-0" :p-ded nil :selected t))
            :to-match-window-tree)))

(describe "pop-to-buffer-same-window"
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
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))

  (it "with 1 window, switching to same purpose reuses window"
    (build-one-window '(:name "xxx-p0-0"))
    (pop-to-buffer-same-window "xxx-p0-1")
    (expect '(:name "xxx-p0-1") :to-match-window-tree))
  (it "with 1 window, switching to other purpose reuses window"
    (build-one-window '(:name "xxx-p0-0"))
    (pop-to-buffer-same-window "xxx-p1-0")
    (expect '(:name "xxx-p1-0") :to-match-window-tree))
  (it "with 2 windows, switching to same purpose reuses window"
    (build-two-windows '((:name "xxx-p0-0" :selected t) (:name "xxx-p1-0")))
    (pop-to-buffer-same-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-1" :selected t)
                    (:name "xxx-p1-0"))
            :to-match-window-tree))
  (it "with 2 windows, switching to other purpose reuses current window"
    (build-two-windows '((:name "xxx-p1-0" :selected t) (:name "xxx-p0-0")))
    (pop-to-buffer-same-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-1" :selected t)
                    (:name "xxx-p0-0"))
            :to-match-window-tree))
  (it "with 2 windows, current window buffer-dedicated, switching to same purpose uses other window"
    (build-two-windows '((:name "xxx-p0-0" :selected t :b-ded t) (:name "xxx-p1-0")))
    (pop-to-buffer-same-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0" :b-ded t)
                    (:name "xxx-p0-1" :selected t))
            :to-match-window-tree))
  (it "with 2 buffer-dedicated windows, switching to same purpose creates new window"
    (build-two-windows '((:name "xxx-p0-0" :selected t :b-ded t) (:name "xxx-p1-0" :b-ded t)))
    (pop-to-buffer-same-window "xxx-p0-1")
    (expect '(split (:name "xxx-p0-0" :b-ded t)
                    (:name "xxx-p0-1" :b-ded nil :selected t)
                    (:name "xxx-p1-0" :b-ded t))
            :to-match-window-tree)))

;; can't test, because "emacs -batch" can't raise frames
(describe "switch-to-buffer-other-frame")

(describe "switch helpers"
  :var (buf1 buf2 buf3)
  (before-each
    (create-buffers "xxx-p0-0" "xxx-p0-1" "xxx-p1-0")
    (setq buf1 (get-buffer "xxx-p0-0")
          buf2 (get-buffer "xxx-p0-1")
          buf3 (get-buffer "xxx-p1-0"))
    (build-one-window '(:name "xxx-p0-0")))

  (describe "purpose-window-buffer-reusable-p"
    (it "returns non-nil only when window contains buffer"
      (expect (purpose-window-buffer-reusable-p nil buf1) :to-be-truthy)
      (expect (purpose-window-buffer-reusable-p nil buf2) :not :to-be-truthy)))

  ;; (ert-deftest purpose-test-display-no-buffer ()
  ;; (ert-deftest purpose-test-display-fallback-pop-window ()
  ;; can't test, because "emacs -batch" can't raise frames
  ;; (ert-deftest purpose-test-display-fallback-pop-frame ()
  ;; (ert-deftest purpose-test-display-fallback-error ()
  ;; (ert-deftest purpose-test-display-fallback-nil ()

  (describe "purpose--normalize-width"
    (it "normalizes integers"
      (expect (purpose--normalize-width 5) :to-equal 5))
    (it "normalizes percentages"
      (expect (purpose--normalize-width 0.5) :to-equal (/ (frame-width) 2)))
    (it "returns nil when it receives nil"
      (expect (purpose--normalize-width nil) :to-be nil))
    (it "throws error for negative values")
    (expect (apply-partially 'purpose--normalize-width -5) :to-throw))

  (describe "purpose--normalize-height"
    (it "normalizes integers"
      (expect (purpose--normalize-height 5) :to-equal 5))
    (it "normalizes percentages"
      (expect (purpose--normalize-height 0.5) :to-equal (/ (frame-height) 2)))
    (it "returns nil when it receives nil"
      (expect (purpose--normalize-height nil) :to-be nil))
    (it "throws error for negative values")
    (expect (apply-partially 'purpose--normalize-height -5) :to-throw))

  ;; (ert-deftest purpose-test-display-at ()
  ;; (ert-deftest purpose-test-special-action-sequences ()
  ;; (ert-deftest purpose-cover-select-buffer-without-action-order ()
  ;; (ert-deftest purpose-test-interactive-switch-buffer ()
  ;; (ert-deftest purpose-test-interactive-switch-buffer-with-some-purpose ()
  ;; (ert-deftest purpose-test-temp-actions-1 ()
  ;; (ert-deftest purpose-test-additional-actions-1 ()
  )
