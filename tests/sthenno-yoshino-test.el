;;; sthenno-yoshino-test.el --- Tests for Yoshino -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'sthenno-yoshino)

(ert-deftest sthenno-yoshino-personality-has-initial-self ()
  (let ((personality (sthenno-yoshino-personality)))
    (should (equal (alist-get 'name personality) "氷芽川四糸乃"))
    (should (equal (alist-get 'status personality) "静かな時間の中"))
    (should (string-match-p "凍っていない" (alist-get 'description personality)))))

(ert-deftest sthenno-yoshino-system-prompt-uses-personality ()
  (let ((prompt (sthenno-yoshino-system-prompt)))
    (should (string-match-p "Speak like 氷芽川四糸乃" prompt))
    (should (string-match-p "emotionally restrained" prompt))
    (should (string-match-p "静かな時間の中" prompt))))

(ert-deftest sthenno-yoshino-observe-current-buffer ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"hi\")")
    (goto-char (point-min))
    (let ((obs (sthenno-yoshino-observe)))
      (should (equal (alist-get 'buffer obs) (buffer-name)))
      (should (equal (alist-get 'major-mode obs) "emacs-lisp-mode"))
      (should (equal (alist-get 'symbol-at-point obs) "message"))
      (should (string-match-p "message" (alist-get 'snippet obs))))))

(ert-deftest sthenno-yoshino-initializes-denote-memory ()
  (let* ((dir (make-temp-file "yoshino-denote-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((files (sthenno-yoshino-initialize)))
          (should (file-exists-p (alist-get 'personality files)))
          (should (file-exists-p (alist-get 'diary files)))
          (should (string-match-p "__yoshino" (file-name-nondirectory
                                               (alist-get 'personality files))))
          (with-temp-buffer
            (insert-file-contents (alist-get 'personality files))
            (should (search-forward "氷芽川四糸乃" nil t))
            (should (search-forward "少し... 寂しい" nil t)))
          (with-temp-buffer
            (insert-file-contents (alist-get 'diary files))
            (should (search-forward "Yoshino woke quietly inside Emacs." nil t))))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-diary-appends-entry ()
  (let* ((dir (make-temp-file "yoshino-diary-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((file (sthenno-yoshino-write-diary "I noticed the first snow.")))
          (should (file-exists-p file))
          (let ((same-file (sthenno-yoshino-write-diary "I stayed nearby.")))
            (should (equal file same-file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "I noticed the first snow." nil t))
            (should (search-forward "I stayed nearby." nil t))))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-step-requests-one-decision ()
  (sthenno-yoshino-reset)
  (let* ((dir (make-temp-file "yoshino-step-" t))
         (sthenno-yoshino-denote-directory dir)
         captured-prompt captured-system captured-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (prompt &rest plist)
                     (setq captured-prompt prompt
                           captured-system (plist-get plist :system)
                           captured-callback (plist-get plist :callback))
                     (funcall captured-callback
                              "{\"action\":\"diary\",\"text\":\"I listened.\"}"
                              nil)
                     :sent)))
          (should (eq (sthenno-yoshino-step) :sent))
          (should (string-match-p "Observation" captured-prompt))
          (should (string-match-p "Speak like 氷芽川四糸乃" captured-system))
          (should (functionp captured-callback)))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-diary-action-is-async ()
  (let* ((dir (make-temp-file "yoshino-async-diary-" t))
         (sthenno-yoshino-denote-directory dir)
         done result)
    (unwind-protect
        (progn
          (sthenno-yoshino-handle-decision-async
           "{\"action\":\"diary\",\"text\":\"I moved later.\"}"
           (lambda (value)
             (setq result value
                   done t)))
          (should-not (directory-files dir nil "\\.org\\'"))
          (while (not done)
            (accept-process-output nil 0.01))
          (should (string-match-p "diary" result))
          (should (directory-files dir nil "--yoshino-diary__.*\\.org\\'")))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-step-reflects-after-async-action ()
  (sthenno-yoshino-reset)
  (let* ((dir (make-temp-file "yoshino-reflect-" t))
         (sthenno-yoshino-denote-directory dir)
         (sthenno-yoshino-reflect-after-action t)
         (calls 0)
         prompts)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (prompt &rest plist)
                     (cl-incf calls)
                     (push prompt prompts)
                     (let ((callback (plist-get plist :callback)))
                       (pcase calls
                         (1 (funcall callback
                                     "{\"action\":\"diary\",\"text\":\"I observed softly.\"}"
                                     nil)
                            :decision)
                         (2 (funcall callback
                                     "{\"reflection\":\"Observation became a small memory.\"}"
                                     nil)
                            :reflection)
                         (_ (error "Unexpected gptel call")))))))
          (should (eq (sthenno-yoshino-step) :decision))
          (while (or sthenno-yoshino--request-active
                     sthenno-yoshino--action-active
                     sthenno-yoshino--reflection-active
                     (< calls 2))
            (accept-process-output nil 0.01))
          (should (= calls 2))
          (should (cl-some (lambda (prompt)
                             (string-match-p "Reflection" prompt))
                           prompts))
          (let ((reflection (car (directory-files
                                  dir t "--yoshino-reflection__.*\\.org\\'"))))
            (should reflection)
            (with-temp-buffer
              (insert-file-contents reflection)
              (should (search-forward "Observation became a small memory."
                                      nil t)))))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-handles-diary-decision ()
  (let* ((dir (make-temp-file "yoshino-decision-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((result (sthenno-yoshino-handle-decision
                       "{\"action\":\"diary\",\"text\":\"I saw a buffer.\"}")))
          (should (string-match-p "diary" result))
          (should (directory-files dir nil "__yoshino.*\\.org\\'")))
      (delete-directory dir t))))

;;; sthenno-yoshino-test.el ends here
