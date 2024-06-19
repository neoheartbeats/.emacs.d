;;; init-projects.el --- Project management -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:


;; Git client using Magit
(use-package magit
  :straight t
  :config (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))

;; (use-package diff-hl
;;   :straight t
;;   :config (global-diff-hl-mode 1))


;; Project management using Projectile
;; (use-package projectile
;;   :straight t
;;   :diminish (projectile-mode)
;;   :config
;;   (projectile-mode 1)
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))


(provide 'init-projects)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
