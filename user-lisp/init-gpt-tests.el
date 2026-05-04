;;; init-gpt-tests.el --- Tests for Hermit helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)

(defmacro use-package (&rest _args) nil)

(defconst sthenno/hermit-test--source-file
  (expand-file-name "init-gpt.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(load sthenno/hermit-test--source-file
      nil t)

(defun sthenno/hermit-test--controller (plist)
  (apply #'sthenno/hermit--make-controller plist))

(defmacro sthenno/hermit-test-with-controller (plist &rest body)
  (declare (indent 1))
  `(let ((old-controller sthenno/hermit--controller)
         (old-mode sthenno/hermit-mode)
         (old-options (copy-sequence sthenno/hermit-options)))
     (unwind-protect
         (progn
           (setq sthenno/hermit--controller
                 (sthenno/hermit-test--controller ,plist))
           ,@body)
       (setq sthenno/hermit-mode old-mode
             sthenno/hermit-options old-options
             sthenno/hermit--controller old-controller))))

(ert-deftest sthenno/hermit-test-state-place-supports-gv ()
  (sthenno/hermit-test-with-controller '(:message-timer 1 :queued-bubble nil)
                                       (setf (sthenno/hermit--state :message-timer) 4)
                                       (cl-incf (sthenno/hermit--state :message-timer) 3)
                                       (push 'x (sthenno/hermit--state :queued-bubble))
                                       (setf (sthenno/hermit--opt :gap) 9)
                                       (cl-incf (sthenno/hermit--opt :gap) 2)
                                       (should (= (sthenno/hermit--state :message-timer) 7))
                                       (should (equal (sthenno/hermit--state :queued-bubble) '(x)))
                                       (should (= (sthenno/hermit--opt :gap) 11))))

(ert-deftest sthenno/hermit-test-options-defaults-follow-defcustoms ()
  (let ((old-options (copy-sequence sthenno/hermit-options)))
    (unwind-protect
        (progn
          (setq sthenno/hermit-options (sthenno/hermit--make-options))
          (dolist (cell sthenno/hermit--opt-vars)
            (should (equal (sthenno/hermit--opt (car cell))
                           (symbol-value (cdr cell))))))
      (setq sthenno/hermit-options old-options))))

(ert-deftest sthenno/hermit-test-text-normalize-and-wrap ()
  (let ((text (propertize " \t hi\nthere\r\n  friend\t " 'face 'bold)))
    (should (equal (sthenno/hermit--plain text) "hi there friend"))
    (should (sthenno/hermit--blank-string-p "\n\t \r"))
    (should-not (sthenno/hermit--plain 42)))
  (should-not (sthenno/hermit--wrap "" 10 2))
  (should (equal (sthenno/hermit--wrap "abcdef" 0 0) '("abc…")))
  (should (equal (sthenno/hermit--wrap "abcdef ghijkl mnop" 6 2)
                 '("abcdef" "ghijk…"))))

(ert-deftest sthenno/hermit-test-number-helper-is-plain-arity ()
  (should (= (sthenno/hermit--n 5 1) 5))
  (should (= (sthenno/hermit--n nil 1 7) 7))
  (should (= (sthenno/hermit--n 120 0 0 100) 100))
  (should-error (sthenno/hermit--n 1 0 0 10 99)
                :type 'wrong-number-of-arguments))

(ert-deftest sthenno/hermit-test_position_and_bubble_layout_are_safe ()
  (should (equal (sthenno/hermit--pet-position 'inside-bottom-right
                                               '(100 . 80) '(20 . 10) 5)
                 '(75 . 65)))
  (should (equal (sthenno/hermit--pet-position 'outside-right-bottom
                                               '(100 . 80) '(20 . 10) 5)
                 '(105 . 65)))
  (should (equal (sthenno/hermit--pet-position 'outside-left-bottom
                                               '(100 . 80) '(20 . 10) 5)
                 '(-25 . 65)))
  (should (equal (sthenno/hermit--pet-position 'center
                                               '(100 . 80) '(20 . 10) 5)
                 '(40 . 35)))
  (should (equal (sthenno/hermit--pet-position 'unknown
                                               '(100 . 80) '(20 . 10) 5)
                 '(75 . 65)))
  (sthenno/hermit-test-with-controller nil
                                       (setf (sthenno/hermit--opt :bubble-max-columns) 4
                                             (sthenno/hermit--opt :bubble-max-lines) 1
                                             (sthenno/hermit--opt :bubble-min-columns) -10
                                             (sthenno/hermit--opt :bubble-padding-columns) -3
                                             (sthenno/hermit--opt :bubble-padding-lines) -2
                                             (sthenno/hermit--opt :bubble-border-width) -10)
                                       (let* ((layout (sthenno/hermit--bubble-layout "abcdef" (selected-frame)))
                                              (content (plist-get layout :content-size)))
                                         (should (equal (plist-get layout :plain-lines) '("abc…")))
                                         (should (equal (plist-get layout :outer-size) content))
                                         (should (> (car content) 0))
                                         (should (> (cdr content) 0)))))

(ert-deftest sthenno/hermit-test-frame-display-is-inert ()
  (let* ((params (sthenno/hermit--common-frame-parameters
                  (selected-frame) "sthenno/hermit-test" '(20 . 10)))
         visible modified raised focused)
    (should (eq (alist-get 'no-focus-on-map params) t))
    (should (eq (alist-get 'no-accept-focus params) t))
    (cl-letf (((symbol-function 'modify-frame-parameters)
               (lambda (frame frame-params)
                 (setq modified (list frame frame-params))))
              ((symbol-function 'make-frame-visible)
               (lambda (frame) (setq visible frame)))
              ((symbol-function 'raise-frame)
               (lambda (frame) (setq raised frame)))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame &optional _norecord) (setq focused frame))))
      (sthenno/hermit--display-frame-inert 'frame)
      (should (equal modified
                     '(frame ((no-focus-on-map . t)
                              (no-accept-focus . t)))))
      (should (eq visible 'frame))
      (should-not raised)
      (should-not focused))))

(ert-deftest sthenno/hermit-test-defcustoms-sync-to-options ()
  (let ((old-options (copy-sequence sthenno/hermit-options))
        (old-height sthenno/hermit-image-height)
        (old-columns sthenno/hermit-bubble-max-columns)
        (old-dnd sthenno/hermit-enable-dnd))
    (unwind-protect
        (progn
          (setq sthenno/hermit-image-height 123
                sthenno/hermit-bubble-max-columns 17
                sthenno/hermit-enable-dnd nil)
          (sthenno/hermit--sync-options)
          (should (= (sthenno/hermit--opt :image-height) 123))
          (should (= (sthenno/hermit--opt :bubble-max-columns) 17))
          (should-not (sthenno/hermit--opt :enable-dnd))
          (setf (sthenno/hermit--opt :bubble-max-columns) 29)
          (should (= sthenno/hermit-bubble-max-columns 29))
          (should (= (sthenno/hermit--opt :bubble-max-columns) 29)))
      (setq sthenno/hermit-options old-options
            sthenno/hermit-image-height old-height
            sthenno/hermit-bubble-max-columns old-columns
            sthenno/hermit-enable-dnd old-dnd))))

(ert-deftest sthenno/hermit-test-command-parser ()
  (let ((command (sthenno/hermit--parse-command "say hello there")))
    (should (eq (sthenno/hermit-command-action command) 'say))
    (should (equal (sthenno/hermit-command-text command) "hello there")))
  (let ((command (sthenno/hermit--parse-command "place center")))
    (should (eq (sthenno/hermit-command-action command) 'place))
    (should (eq (sthenno/hermit-command-placement command) 'center)))
  (let ((command (sthenno/hermit--parse-command "option bubble-timeout 2.5")))
    (should (eq (sthenno/hermit-command-action command) 'option))
    (should (eq (sthenno/hermit-command-option command) :bubble-timeout))
    (should (= (sthenno/hermit-command-value command) 2.5)))
  (let ((command (sthenno/hermit--parse-command "run printf hello")))
    (should (eq (sthenno/hermit-command-action command) 'run))
    (should (equal (sthenno/hermit-command-shell-command command)
                   "printf hello")))
  (should-error (sthenno/hermit--parse-command "jump now") :type 'user-error)
  (should-error (sthenno/hermit--parse-command "place nowhere") :type 'user-error))

(ert-deftest sthenno/hermit-test-process-filter-and-sentinel ()
  (let (said)
    (let ((record (sthenno/hermit--make-process-record
                   :process 'proc
                   :command "printf hello")))
      (sthenno/hermit-test-with-controller
       `(:status ready :process-record ,record)
       (cl-letf (((symbol-function 'sthenno/hermit-say)
                  (lambda (text) (push text said))))
         (sthenno/hermit--process-filter 'proc "hello")
         (sthenno/hermit--process-filter 'proc " world")
         (should (equal (sthenno/hermit-process-record-output record)
                        "hello world"))
         (should (equal said '("hello world" "hello")))
         (sthenno/hermit--process-sentinel 'proc "finished\n")
         (should-not (sthenno/hermit--state :process-record))
         (should (equal (car said) "run finished: printf hello")))))))

(ert-deftest sthenno/hermit-test-teardown-cancels-process ()
  (let ((record (sthenno/hermit--make-process-record
                 :process 'proc
                 :command "sleep 1"))
        deleted)
    (sthenno/hermit-test-with-controller
     `(:process-record ,record)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (process) (eq process 'proc)))
               ((symbol-function 'delete-process)
                (lambda (process) (push process deleted))))
       (sthenno/hermit--teardown-controller)
       (should (equal deleted '(proc)))
       (should-not sthenno/hermit--controller)))))

(ert-deftest sthenno/hermit-test-image-size-scaling ()
  (should (equal (sthenno/hermit--scale-image-size '(535 . 1080) 100
                                                   "/tmp/hmt.png")
                 '(50 . 100)))
  (should-error (sthenno/hermit--scale-image-size '(535 . 0) 100
                                                  "/tmp/hmt.png")))

(ert-deftest sthenno/hermit-test-enable-uses-sync-image-size ()
  (let (made visible rendered hooks)
    (sthenno/hermit-test-with-controller nil
                                         (setf (sthenno/hermit--opt :image-height) 420)
                                         (cl-letf (((symbol-function 'display-graphic-p) (lambda (_frame) t))
                                                   ((symbol-function 'file-readable-p) (lambda (_file) t))
                                                   ((symbol-function 'clear-image-cache) (lambda (&rest _) nil))
                                                   ((symbol-function 'make-process)
                                                    (lambda (&rest _) (error "make-process should not run")))
                                                   ((symbol-function 'sthenno/hermit--raw-image-size)
                                                    (lambda (_file) '(535 . 1080)))
                                                   ((symbol-function 'frame-live-p)
                                                    (lambda (frame) (eq frame 'parent-frame)))
                                                   ((symbol-function 'sthenno/hermit--pet-frame-parameters)
                                                    (lambda (&rest _) '((no-accept-focus . t))))
                                                   ((symbol-function 'make-frame)
                                                    (lambda (params) (setq made params) 'pet-frame))
                                                   ((symbol-function 'sthenno/hermit--render-pet)
                                                    (lambda () (setq rendered t)))
                                                   ((symbol-function 'sthenno/hermit--refresh-placement)
                                                    (lambda () nil))
                                                   ((symbol-function 'sthenno/hermit--display-frame-inert)
                                                    (lambda (frame) (setq visible frame)))
                                                   ((symbol-function 'sthenno/hermit--hooks)
                                                    (lambda (action) (setq hooks action)))
                                                   ((symbol-function 'sthenno/hermit--flush-queued-bubble)
                                                    (lambda () nil)))
                                           (sthenno/hermit--enable "/tmp/hmt.png" 'parent-frame)
                                           (should (equal made '((no-accept-focus . t))))
                                           (should (eq (sthenno/hermit--state :status) 'ready))
                                           (should (equal (sthenno/hermit--state :pet-size) '(208 . 420)))
                                           (should (eq (sthenno/hermit--state :pet-frame) 'pet-frame))
                                           (should (eq visible 'pet-frame))
                                           (should rendered)
                                           (should hooks)))))

(ert-deftest sthenno/hermit-test-buffer-helper-returns-buffer ()
  (let (buffer)
    (unwind-protect
        (sthenno/hermit-test-with-controller nil
                                             (setq buffer
                                                   (sthenno/hermit--buffer
                                                    :image-buffer " *sthenno/hermit-test-buffer*"
                                                    sthenno/hermit-image-mode-map))
                                             (should (bufferp buffer))
                                             (should (eq (sthenno/hermit--state :image-buffer) buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest sthenno/hermit-test-render-bubble-uses-new-frame-not-setter-result ()
  (let (paint-frame visible-frame)
    (sthenno/hermit-test-with-controller '(:parent-frame parent-frame)
                                         (cl-letf (((symbol-function 'frame-live-p)
                                                    (lambda (frame) (eq frame 'parent-frame)))
                                                   ((symbol-function 'sthenno/hermit--bubble-layout)
                                                    (lambda (_text _parent)
                                                      '(:render-lines ("hello")
                                                                      :content-size (40 . 20)
                                                                      :outer-size (42 . 22))))
                                                   ((symbol-function 'sthenno/hermit--bubble-frame-parameters)
                                                    (lambda (&rest _) '((no-accept-focus . t))))
                                                   ((symbol-function 'make-frame)
                                                    (lambda (_params) 'bubble-frame))
                                                   ((symbol-function 'sthenno/hermit--paint)
                                                    (lambda (frame &rest _args) (setq paint-frame frame)))
                                                   ((symbol-function 'modify-frame-parameters)
                                                    (lambda (&rest _) nil))
                                                   ((symbol-function 'set-frame-size)
                                                    (lambda (&rest _) nil))
                                                   ((symbol-function 'sthenno/hermit--place-bubble)
                                                    (lambda () nil))
                                                   ((symbol-function 'sthenno/hermit--display-frame-inert)
                                                    (lambda (frame) (setq visible-frame frame))))
                                           (sthenno/hermit--render-bubble "hello")
                                           (should (eq (sthenno/hermit--state :bubble-frame) 'bubble-frame))
                                           (should (eq paint-frame 'bubble-frame))
                                           (should (eq visible-frame 'bubble-frame))))))

(ert-deftest sthenno/hermit-test-say-queue-and-echo-coalesce ()
  (let (rendered delivered idle-calls cancelled)
    (sthenno/hermit-test-with-controller '(:status probing)
                                         (cl-letf (((symbol-function 'sthenno/hermit--render-bubble)
                                                    (lambda (_text) (setq rendered t))))
                                           (sthenno/hermit-say "  hello\nthere  ")
                                           (should-not rendered)
                                           (should (equal (sthenno/hermit--state :queued-bubble)
                                                          "hello there"))
                                           (sthenno/hermit-clear-bubble)
                                           (should-not (sthenno/hermit--state :queued-bubble))))
    (sthenno/hermit-test-with-controller
     '(:status ready :queued-bubble "hello")
     (cl-letf (((symbol-function 'sthenno/hermit-say)
                (lambda (text) (setq delivered text))))
       (sthenno/hermit--flush-queued-bubble)
       (should (equal delivered "hello"))
       (should-not (sthenno/hermit--state :queued-bubble))))
    (sthenno/hermit-test-with-controller
     '(:status ready :pet-frame pet :message-timer old-timer)
     (setq sthenno/hermit-mode t)
     (cl-letf (((symbol-function 'sthenno/hermit--live-frame)
                (lambda (key) (and (eq key :pet-frame) 'pet)))
               ((symbol-function 'timerp)
                (lambda (timer) (memq timer '(old-timer new-timer))))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (push timer cancelled)))
               ((symbol-function 'run-with-idle-timer)
                (lambda (&rest args) (push args idle-calls) 'new-timer)))
       (sthenno/hermit--message-filter "first")
       (sthenno/hermit--message-filter "second")
       (should (equal cancelled '(new-timer old-timer)))
       (should (= (length idle-calls) 2))
       (should (equal (sthenno/hermit--state :pending-message) "second"))
       (should (eq (sthenno/hermit--state :message-timer) 'new-timer))))))

(ert-deftest sthenno/hermit-test-passive-say-does-not-focus ()
  (let (focused)
    (sthenno/hermit-test-with-controller '(:status ready)
                                         (cl-letf (((symbol-function 'sthenno/hermit--render-bubble)
                                                    (lambda (_text) nil))
                                                   ((symbol-function 'select-frame-set-input-focus)
                                                    (lambda (frame &optional _norecord) (setq focused frame))))
                                           (sthenno/hermit-say "hello")
                                           (should-not focused)))))

(ert-deftest sthenno/hermit-test-dnd-dispatch ()
  (let (file-args)
    (cl-letf (((symbol-function 'dnd-begin-file-drag)
               (lambda (&rest args) (setq file-args args) 'copy)))
      (should (eq (sthenno/hermit--begin-drag-file 'frame "/tmp/hmt.png")
                  'copy))
      (should (equal file-args '("/tmp/hmt.png" frame copy t)))))
  (let (text-args)
    (cl-letf (((symbol-function 'dnd-begin-text-drag)
               (lambda (&rest args) (setq text-args args) 'copy)))
      (should (eq (sthenno/hermit--begin-drag-text 'frame "hello")
                  'copy))
      (should (equal text-args '("hello" frame copy t))))))

(ert-deftest sthenno/hermit-test-menu-and-iconify_focus_policy ()
  (let (iconified)
    (cl-letf (((symbol-function 'sthenno/hermit-iconify-parent)
               (lambda () (setq iconified t))))
      (should-not (sthenno/hermit--dispatch-menu-choice 'iconify))
      (should iconified)))
  (cl-letf (((symbol-function 'sthenno/hermit-clear-bubble)
             (lambda () nil)))
    (should (sthenno/hermit--dispatch-menu-choice 'clear)))
  (cl-letf (((symbol-function 'frame-parent)
             (lambda (frame) (and (eq frame 'leaf) 'root))))
    (should (eq (sthenno/hermit--root-frame 'leaf) 'root))))

(provide 'init-gpt-tests)
;;; init-gpt-tests.el ends here
