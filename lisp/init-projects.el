;;; init-projects.el --- Project management -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 KAMUSUSANOWO

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Git client Magit
;;
(use-package magit
  :straight t
  :config (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))

;;
;; Project management
;;
(use-package projectile
  :straight t
  :config (projectile-mode 1))

(provide 'init-projects)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
