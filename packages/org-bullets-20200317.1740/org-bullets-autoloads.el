;;; org-bullets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-bullets" "org-bullets.el" (0 0 0 0))
;;; Generated autoloads from org-bullets.el

(autoload 'org-bullets-mode "org-bullets" "\
Use UTF8 bullets in Org mode headings.

If called interactively, enable Org-Bullets mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bullets" '("org-bullets-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-bullets-autoloads.el ends here
