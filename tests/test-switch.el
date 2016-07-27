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

;;; --- extract window-recipe-like trees from `window-tree'
(defvar window-data-extractors
  (list :name (lambda (win) (buffer-name (window-buffer win)))
        :purpose #'purpose-window-purpose
        :selected (lambda (win) (eq win (frame-selected-window win)))
        :b-ded #'window-dedicated-p
        :p-ded #'purpose-window-purpose-dedicated-p))

(defun map-plist (func plist)
  "Apply FUNC to each key-value pair in PLIST.
FUNC is called as (FUNC key value) for each pair."
  (do* ((plist plist (cddr plist))
        (key (car plist) (car plist))
        (value (cadr plist) (cadr plist))
        (result))
      ;; `consp' test stops loop for plist values of `()' and also `(:a)', IOW
      ;; ignores last element if plist has an odd (not even) length
      ((not (consp (cdr plist)))
       (nreverse result))
    (push (funcall func key value) result)))

(defun alist-to-plist (alist)
  (nreverse (seq-reduce
             (lambda (result next-pair)
               (cons (cdr next-pair) (cons (car next-pair) result)))
             alist nil)))

(defun extract-window-data (window)
  (alist-to-plist (map-plist (lambda (key extractor)
                               (cons key (funcall extractor window)))
                             window-data-extractors)))

(defun extract-window-tree-1 (tree)
  (if (windowp tree)
      (extract-window-data tree)
    ;; FUTURE: can replace 'split with `extract-split-data' (or something)
    (cons 'split (seq-map #'extract-window-tree-1 (cddr tree)))))

(defun extract-window-tree (&optional frame)
  (extract-window-tree-1 (car (window-tree frame))))

;;; ---
;;; --- window-recipe matcher
(defvar window-data-comparers
  (list :name #'string=
        :purpose #'eq
        :selected #'eq
        :b-ded #'eq
        :p-ded #'eq))

(defun plist-to-alist (plist)
  (map-plist #'cons plist))

(defun window-matches-recipe-p (window recipe)
  (cl-loop for (key . expect-val) in (plist-to-alist recipe)
           for comparer = (plist-get window-data-comparers key)
           for val = (plist-get window key)
           unless comparer do (error "Unknown comparer %S" key)
           always (funcall comparer val expect-val)))

(defun tree-matches-recipe-p (tree recipe)
  (if (eq (car recipe) 'split)
      ;; should be split
      (and (eq (car tree) 'split)
           (= (length tree) (length recipe))
           (cl-loop for sub-tree in (cdr tree)
                    for sub-recipe in (cdr recipe)
                    always (tree-matches-recipe-p sub-tree sub-recipe)))
    ;; should be window
    (and (not (eq (car tree) 'split))
         (window-matches-recipe-p tree recipe))))

(buttercup-define-matcher :to-match-window-recipe (tree recipe)
  (if (tree-matches-recipe-p tree recipe)
      (cons t (format "Expcted window tree %S to match recipe %S" tree recipe))
    (cons nil (format "Expected window tree %S to be different from recipe %S" tree recipe))))

(buttercup-define-matcher :to-match-window-tree (recipe)
  (let ((tree (extract-window-tree)))
    (if (tree-matches-recipe-p tree recipe)
        (cons t (format "Expcted window recipe %S to match tree %S" recipe tree))
      (cons nil (format "Expected window recipe %S to be different from tree %S" recipe tree)))))

;;; ---

;;; --- obsolete?
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
;;; ---

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
