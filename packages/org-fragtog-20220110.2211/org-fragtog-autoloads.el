;;; org-fragtog-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-fragtog" "org-fragtog.el" (0 0 0 0))
;;; Generated autoloads from org-fragtog.el

(autoload 'org-fragtog-mode "org-fragtog" "\
A minor mode that automatically toggles Org mode LaTeX fragment previews.
Fragment previews are disabled for editing when your cursor steps onto them,
and re-enabled when the cursor leaves.

If called interactively, enable Org-Fragtog mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-fragtog" '("org-fragtog-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-fragtog-autoloads.el ends here
