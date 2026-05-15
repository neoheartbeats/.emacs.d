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

;;; sthenno-yoshino-test.el ends here
