;;; init-temp.el --- Configure Tempel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'yasnippet)
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets/" user-emacs-directory)))
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)

  (yas-global-mode 1)
  (with-eval-after-load 'yasnippet
    (diminish 'yas-minor-mode)))


(require 'abbrev)

(let ((table global-abbrev-table))
  (define-abbrev table "zmail" "ilyaw3939@gmail.com")
  (define-abbrev table "latex" "LaTeX")
  (define-abbrev table "github" "GitHub")
  (define-abbrev table "gitlab" "GitLab")
  (define-abbrev table "emacsconf" "EmacsConf")
  (define-abbrev table "auctex" "AUCTeX"))

(set-default 'abbrev-mode t)
(diminish 'abbrev-mode)


(provide 'init-temp)
;;; init-temp ends here
