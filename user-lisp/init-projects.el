;;; init-projects.el --- Project management -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains project navigation and VCS tools.

;;; Code:

(use-package magit
  :ensure t
  :config (setopt magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))

(use-package xref
  :ensure nil
  :init (setopt xref-search-program 'ripgrep)
  :bind (:map global-map
              ("M-/" . xref-find-references)))

(provide 'init-projects)
