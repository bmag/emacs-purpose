;; -*- lexical-binding: t -*-
(require 'buttercup-init)

(defvar test-config-purpose-config
  `((purpose-user-name-purposes . (("FOO" . USER1)))
    (purpose-user-regexp-purposes . (("^FOO2\\'" . USER2)))
    (purpose-user-mode-purposes . ((text-mode . USER3)))
    (purpose-extended-configuration . (:test
                                       ,(purpose-conf "test-conf"
                                                      :name-purposes '(("TEST" . TEST2)))))
    (purpose-use-default-configuration . t)))

(describe "Temporary Purpose Changes"
  :var (config-snapshot config-to-load)
  (before-all
    (setq config-to-load '((purpose-user-name-purposes . (("TEST" . NEW1)))
                           (purpose-user-regexp-purposes . nil)
                           (purpose-user-mode-purposes . ((text-mode . NEW2)))
                           (purpose-extended-configuration . nil)
                           (purpose-use-default-configuration . nil)))
    (setq config-snapshot (get-purpose-config)))
  (before-each
    (load-purpose-config test-config-purpose-config))
  (after-each (load-purpose-config config-snapshot))

  ;;; temporary purpose changers:
  ;; purpose-save-purpose-config - is tested by the other tests
  ;; purpose-with-temp-purposes   <--DONE--
  ;; purpose-with-empty-purposes   <--DONE--
  ;; purpose-with-additional-purposes   <--DONE--
  (it "purpose-with-temp-purposes restores config when finished"
    (purpose-with-temp-purposes '(("TEST" . NEW1)) nil '((text-mode . NEW2))
      t)
    (expect (get-purpose-config) :to-equal test-config-purpose-config))
  (it "purpose-with-temp-purposes changes config while executing body"
    (purpose-with-temp-purposes '(("TEST" . NEW1)) nil '((text-mode . NEW2))
      (expect (get-purpose-config) :to-equal config-to-load)))
  (it "purpose-with-empty-purposes restores config when finished"
    (purpose-with-empty-purposes t)
    (expect (get-purpose-config) :to-equal test-config-purpose-config))
  (it "purpose-with-empty-purposes clears config while executing body"
    (purpose-with-empty-purposes
      (expect (get-purpose-config) :to-equal '((purpose-user-name-purposes . nil)
                                               (purpose-user-regexp-purposes . nil)
                                               (purpose-user-mode-purposes . nil)
                                               (purpose-extended-configuration . nil)
                                               (purpose-use-default-configuration . nil)))))
  (it "purpose-with-additional-purposes restores config when finished"
    (purpose-with-additional-purposes '(("TEST" . NEW1)) nil '((text-mode . NEW2))
      t)
    (expect (get-purpose-config) :to-equal test-config-purpose-config))
  (it "purpose-with-additional-purposes adds config while executing body"
    (purpose-with-additional-purposes '(("TEST" . NEW1)) nil '((text-mode . NEW2))
      (expect (get-purpose-config) :to-equal
              (mapcar (lambda (sym)
                        (cons sym (append (purpose-alist-get sym config-to-load)
                                          (purpose-alist-get sym test-config-purpose-config))))
                      (mapcar #'car config-to-load))))))

(describe "purpose-validate-conf"
  (it "modes, names and regexps must be alists"
    (expect (apply-partially 'purpose-validate-conf nil nil nil) :not :to-throw)
    (expect (apply-partially 'purpose-validate-conf 0 nil nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil 0 nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil nil 0) :to-throw)
    (expect (apply-partially 'purpose-validate-conf '(a b) nil nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil '("a" "b") nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil nil '("a" "b")) :to-throw)
    (expect (apply-partially 'purpose-validate-conf '((a . a)) '(("a" . a)) '(("a" . a))) :not :to-throw))
  (it "each mode must be a symbol"
    (expect (apply-partially 'purpose-validate-conf '(("a" . b)) nil nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf '((a . b)) nil nil) :not :to-throw))
  (it "each mode must map to a symbol"
    (expect (apply-partially 'purpose-validate-conf '((a . "b")) nil nil) :to-throw))
  (it "each name must be a string"
    (expect (apply-partially 'purpose-validate-conf nil '((a . b)) nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil '(("a" . b)) nil) :not :to-throw))
  (it "each name must map to a symbol"
    (expect (apply-partially 'purpose-validate-conf nil '(("a" . "b")) nil) :to-throw))
  (it "each regexp must be a string"
    (expect (apply-partially 'purpose-validate-conf nil '((a . b)) nil) :to-throw)
    (expect (apply-partially 'purpose-validate-conf nil '(("a" . b)) nil) :not :to-throw))
  (it "each regexp must map to a symbol"
    (expect (apply-partially 'purpose-validate-conf nil nil '(("a" . "b"))) :to-throw)))

;;; variables:
;; purpose-use-default-configuration   <--TODO--

;;; will be obsolete
;; purpose--fill-hash   <--X--
;; purpose--set-and-compile-configuration   <--X--
;; purpose-conf-add-purposes   <--X--
;; purpose-conf-remove-purposes   <--X--
;; purpose-compile-default-configuration   <--X--

;;; backward compatibility
;; purpose-compile-user-configuration   <--TODO--
;; purpose-compile-extended-configuration   <--TODO--
;; purpose-set-extension-configuration   <--TODO--
;; purpose-get-extension-configuration   <--TODO--
;; purpose-del-extension-configuration   <--TODO--
;; purpose-add-extension-purposes   <--TODO--
;; purpose-remove-extension-purposes   <--TODO--
;; purpose-add-user-purposes   <--TODO--
;; purpose-remove-user-purposes   <--TODO--
