;;; init-projects.el --- Project management -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;

;;; Code:
;;

(setq project-prompter #'project-prompt-project-name)
;; (setq project-mode-line t)

;;; Git client using Magit

(use-package magit
  :ensure t
  :config (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))

;;; Xref

(use-package xref
  :init (setq xref-search-program 'ripgrep)
  :bind (:map global-map
              ("M-/" . xref-find-references)))

(provide 'init-projects)

;;; init-projects.el ends here
