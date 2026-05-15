;;; sthenno-yoshino-test.el --- Tests for Yoshino -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'sthenno-yoshino)

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

(ert-deftest sthenno-yoshino-registers-existing-function ()
  (sthenno-yoshino-reset-workspace)
  (let ((skill (sthenno-yoshino-register-skill
                'buffer-name 'read 'none "Return the current buffer name.")))
    (should (equal (plist-get skill :name) "buffer-name"))
    (should (eq (plist-get skill :symbol) 'buffer-name))
    (should (eq (plist-get skill :risk) 'read))
    (should (gethash "buffer-name" (sthenno-yoshino-skills)))))

(ert-deftest sthenno-yoshino-calls-read-skill ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'buffer-name 'read 'none)
  (with-temp-buffer
    (rename-buffer "yoshino-test-buffer" t)
    (should (equal (sthenno-yoshino-call-skill "buffer-name")
                   "yoshino-test-buffer"))))

(ert-deftest sthenno-yoshino-blocks-write-skill-when-confirmation-declines ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'ignore 'write 'string)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
    (let ((sthenno-yoshino-confirm-write-actions t))
      (should-error (sthenno-yoshino-call-skill "ignore" "nope")
                    :type 'user-error))))

(ert-deftest sthenno-yoshino-writes-diary-note ()
  (let* ((dir (make-temp-file "yoshino-denote-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((file (sthenno-yoshino-write-diary "I noticed my first trace.")))
          (should (file-exists-p file))
          (should (string-match-p "__yoshino" (file-name-nondirectory file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "I noticed my first trace." nil t))))
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

(ert-deftest sthenno-yoshino-handles-call-decision ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'buffer-name 'read 'none)
  (with-temp-buffer
    (rename-buffer "decision-buffer" t)
    (should (equal (sthenno-yoshino-handle-decision
                    "{\"action\":\"call\",\"skill\":\"buffer-name\",\"args\":{}}")
                   "decision-buffer"))))

(ert-deftest sthenno-yoshino-step-requests-one-decision ()
  (sthenno-yoshino-reset-workspace)
  (let (captured-prompt captured-callback)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest plist)
                 (setq captured-prompt prompt
                       captured-callback (plist-get plist :callback))
                 (funcall captured-callback
                          "{\"action\":\"stop\",\"answer\":\"rest\"}"
                          nil)
                 :sent)))
      (should (eq (sthenno-yoshino-step) :sent))
      (should (string-match-p "Available skills" captured-prompt))
      (should (functionp captured-callback))
      (should (alist-get 'buffer (plist-get (sthenno-yoshino-workspace)
                                            :attention))))))

;;; sthenno-yoshino-test.el ends here
