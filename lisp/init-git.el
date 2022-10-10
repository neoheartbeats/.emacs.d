;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.
;; This file should be placed before loading `init-org-roam.el'.
;; The package `magit' provides functions `org-roam' also needs.

;;; Code:

(use-package magit
  :init
  (setq-default magit-diff-refine-hunk t)
  :custom
  (magit-section-visibility-indicator nil)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch))
  :config
  (with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)))


(provide 'init-git)
;;; init-git.el ends here
