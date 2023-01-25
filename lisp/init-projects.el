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


;; Better file management with `Dirvish'
(use-package dirvish
  :config
  (setq dirvish-hide-details t)
  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line 'global)
  (setq dirvish-depth 0)
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 25)
  (setq dirvish-layout-recipes '((0 0 0.4)
                                 (0 0 0.8)
                                 (1 0.08 0.8)
                                 (1 0.11 0.55)))
  (setq dirvish-header-line-format
        '(
          :left (path)
          :right (free-space))
        dirvish-mode-line-format
        '(
          :left (sort file-time " ")
          :right (omit yank index)))

  (dirvish-override-dired-mode 1))


(provide 'init-projects)
;;; init-projects.el ends here
