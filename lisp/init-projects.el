;;; init-projects.el --- Project management in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Note this file should be loaded before loading `init-org-roam.el',
;; since package `magit.el' provides functions `org-roam.el' also needs.

;;; Code:


;; Git client using `magit'
(use-package magit
  :init
  (setq-default magit-diff-refine-hunk t)
  :custom

  ;; Disable showing the bitmap indicators
  (magit-section-visibility-indicator nil)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))


;; Project management using `projectile.el'
(use-package projectile
  :diminish
  :init
  (when (file-directory-p my-dev-path)
    (let ((project-path-list '()))
      (push my-dev-path project-path-list)
      (setq projectile-project-search-path project-path-list)))

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (projectile-mode 1)
  :bind
  ((:map projectile-mode-map
         ("C-c p" . 'projectile-command-map))))


(provide 'init-projects)
;;; init-projects.el ends here