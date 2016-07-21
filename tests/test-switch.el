;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'seq)
(require 'window-purpose)

(buttercup-define-matcher :to-show-buffers (frame buffer-names)
  (let ((frame-buffers (seq-map (lambda (window)
                                  (buffer-name (window-buffer window)))
                                (window-list frame 'no-minibuffer))))
    (if (equal (sort frame-buffers #'string-lessp)
               (sort buffer-names #'string-lessp))
        (cons t (format "Expcted frame to show buffers %S" buffer-names))
      (cons nil (format "Expcted frame not to show exactly buffers %S" buffer-names)))))

(purpose-mode)

(describe "switch-buffer suite"
  (it "switches a buffer with purpose-switch-buffer"
    (delete-other-windows)
    (set-window-buffer nil (get-buffer-create "xxx-p0-0"))
    (purpose-switch-buffer "xxx-p0-1")
    (expect (selected-frame) :to-show-buffers '("xxx-p0-1"))
    ))
