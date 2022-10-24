;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.
;; This file should be placed before loading `init-org-roam.el'.
;; The package `magit' provides functions `org-roam' also needs.

;;; Code:

(use-package magit
  :defer t
  :init
  (setq-default magit-diff-refine-hunk t)
  :custom ;; Disable showing the bitmap indicators
  (magit-section-visibility-indicator nil)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch))
  :config
  (with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)))


;; Highlighting uncommitted changes in the left fringe
;; (use-package diff-hl
;;   :init
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   :config
;;   (global-diff-hl-mode 1)
;;   (diff-hl-flydiff-mode 1)
;;   :custom
;;   (diff-hl-draw-borders nil))


(provide 'init-git)
;;; init-git.el ends here
