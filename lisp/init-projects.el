;;; init-projects.el --- Project management in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Note this file should be loaded before loading `init-org-roam.el',
;; since package `magit.el' provides functions `org-roam.el' also needs.

;;; Code:


;; Git client using `magit'
(use-package
  magit
  :init (setq-default magit-diff-refine-hunk t)
  :custom


  (magit-section-visibility-indicator nil) ; disable showing the bitmap indicators
  :bind (("C-x g" . magit-status) ("C-x M-g" . magit-dispatch)))


(provide 'init-projects)
;;; init-projects.el ends here
