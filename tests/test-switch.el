;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'seq)
(require 'window-purpose)

(defun get-purpose-config ()
  (seq-map (lambda (var)
             (cons var (eval var)))
           '(purpose-user-mode-purposes
             purpose-user-name-purposes
             purpose-user-regexp-purposes
             purpose-extended-configuration
             purpose-use-default-configuration)))

(cl-defun make-purpose-config (&key modes names regexps extensions use-default)
  (list (cons 'purpose-user-mode-purposes modes)
        (cons 'purpose-user-name-purposes names)
        (cons 'purpose-user-regexp-purposes regexps)
        (cons 'purpose-extended-configuration extensions)
        (cons 'purpose-use-default-configuration use-default)))

(defun load-purpose-config (config)
  (seq-map (lambda (var-value)
             (set (car var-value) (cdr var-value)))
           config)
  (purpose-compile-user-configuration)
  (purpose-compile-extended-configuration)
  (purpose-compile-default-configuration))

(defun frame-buffers (frame)
  (seq-map #'window-buffer (window-list frame 'no-minibuffer)))

(buttercup-define-matcher :to-show-exactly-buffers (frame buffer-names)
  (let ((frame-buffers (seq-map #'buffer-name (frame-buffers frame))))
    (if (equal (sort frame-buffers #'string-lessp)
               (sort buffer-names #'string-lessp))
        (cons t (format "Expcted frame to show exactly %S" buffer-names))
      (cons nil (format "Expcted frame not to show exactly %S" buffer-names)))))

(buttercup-define-matcher :to-show-any-buffers (frame buffer-names)
  (let ((frame-buffs (frame-buffers frame)))
    (if (seq-find (lambda (name)
                    (memq (get-buffer name) frame-buffs))
                  buffer-names)
        (cons t (format "Expected frame to show at least one of %S" buffer-names))
      (cons t (format "Expected frame to show none of %S" buffer-names)))))

(buttercup-define-matcher :to-show-all-buffers (frame buffer-names)
  (let ((frame-buffs (frame-buffers)))
    (if (seq-every-p (lambda (name)
                       (memq (get-buffer name) frame-buffs))
                     buffer-names)
        (cons t (format "Expected frame to show all of %S" buffer-names))
      (cons t (format "Expected frame not to show at least one of %S" buffer-names)))))

(buttercup-define-matcher :to-show-buffer (window buffer-or-name)
  (let ((buff (window-normalize-buffer buffer-or-name))
        (win (window-normalize-window window)))
    (if (eq (window-buffer win) buff)
        (cons t (format "Expected window %S to show %s" win (buffer-name buff)))
      (cons nil (format "Expected window %S not to show %s" win (buffer-name buff))))))

;; TODO: implement `expect-window-config' for matching particular window
;; configurations
;; (defun expect-window-config (frame wconf)
;;   )
;; selected: is selected-window
;; p-ded: purpose-dedicated
;; b-ded: buffer-dedicated
;; params: general window parameters
;; split: generic split, either horizontal or vertical. Split direction usually
;;        depends on screen size, so it's better not to specify it, lest the
;;        test results will depend on the tester's monitor size.
;; (expect-window-config nil
;;   '("buf1" :selected t :params '((purpose-dedicated . t))))
;; (expect-window-config nil
;;   '(split ("buf1" :selected t :p-ded t)
;;           ("buf2" :p-ded nil)))
;; (expect-window-config nil
;;   '(split ("buf1" :selected t :p-ded t)
;;           (split ("buf2" :p-ded nil)
;;                  ("buf3" :b-ded t))))

(purpose-mode)

(describe "switch-buffer suite"
  :var (config-snapshot)
  (before-all
    (setq config-snapshot (get-purpose-config))
    (load-purpose-config
     (make-purpose-config :regexps '(("^xxx-p0-" . p0)
                                     ("^xxx-p1-" . p1)))))
  (after-all
    (load-purpose-config config-snapshot))
  (before-each
    (seq-map #'get-buffer-create
             '("xxx-p0-0" "xxx-p0-1" "xxx-p1-0"))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (set-window-dedicated-p nil nil)
    (purpose-set-window-purpose-dedicated-p nil nil)
    (set-window-buffer nil "xxx-p0-0"))

  (it "switch-to-buffer to same purpose"
    (switch-to-buffer "xxx-p0-1")
    (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-1")))

  (it "switch-to-buffer to other purpose"
    (switch-to-buffer "xxx-p1-0")
    (expect (selected-frame) :to-show-exactly-buffers '("xxx-p1-0")))

  (it "switch-to-buffer from purpose-dedicated to same purpose"
    (purpose-set-window-purpose-dedicated-p nil t)
    (switch-to-buffer "xxx-p0-1")
    (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-1"))
    (expect (purpose-window-purpose-dedicated-p)))

  (it "switch-to-buffer from purpose-dedicated to other purpose"
    (purpose-set-window-purpose-dedicated-p nil t)
    (switch-to-buffer "xxx-p1-0")
    (expect (selected-frame) :to-show-exactly-buffers '("xxx-p0-0" "xxx-p1-0"))
    (expect (selected-window) :to-show-buffer "xxx-p1-0")
    (expect (next-window) :to-show-buffer "xxx-p0-0")
    (expect (purpose-window-purpose-dedicated-p) :to-be nil)
    (expect (purpose-window-purpose-dedicated-p (next-window)) :to-be t))
  )
