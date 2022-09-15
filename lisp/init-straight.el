;;; init-straight.el --- Package pre-loaded process -*- lexical-binding: t -*-
;;; Commentary:

;; We choose to use `straight.el' to replace `package.el'.

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
	     (expand-file-name "straight/repos/straight.el/bootstrap.el"
					               user-emacs-directory))
			(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
			  (url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

;; Install then config `use-package' to format code
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(provide 'init-straight)
;;; init-straight.el ends here
