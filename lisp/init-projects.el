;; init-projects.el --- Project management in Emacs -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Note this file should be loaded before loading `init-org-roam.el',
;; since package `magit.el' provides functions `org-roam.el' also needs.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git client Magit
(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-section-visibility-indicator nil) ; Disable showing the bitmap indicators
  :bind ("C-x g" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Project management
(use-package project
  :diminish t
  :defer t
  :bind
  (("s-p" . project-find-file)
   ("C-c p" . project-switch-project)))


(provide 'init-projects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-projects.el ends here
