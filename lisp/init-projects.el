;;; init-projects.el --- Project management in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Note this file should be loaded before loading `init-org-roam.el',
;; since package `magit.el' provides functions `org-roam.el' also needs.

;;; Code:


;; Git client using `magit'
(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (setq-default magit-section-visibility-indicator nil)

  (define-key global-map (kbd "C-x g") 'magit-status))


(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook #'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " 􀐚")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))


(provide 'init-projects)
;;; init-projects.el ends here
