;;; init-eglot.el --- Tree-sitter, Eglot, and language settings -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains tree-sitter, Eglot, and language-specific settings.

;;; Code:


;;; Prefer the built-in Emacs 31 tree-sitter mode remapping support
(setopt treesit-font-lock-level 4
        treesit-enabled-modes t
        treesit-auto-install-grammar 'always)

;;; Initialize `eglot'
(use-package eglot
  :ensure nil
  :defer t
  :hook ((LaTeX-mode tex-mode python-base-mode) . eglot-ensure))

;;; Python

;; The built-in `python' environment
(use-package python
  :ensure nil
  :init (setq-default python-indent-offset 4
                      python-indent-guess-indent-offset nil
                      python-indent-guess-indent-offset-verbose nil)
  :bind (:map python-base-mode-map
              ("s-<up>"   . python-nav-beginning-of-block)
              ("s-<down>" . python-nav-end-of-block)
              ("C-x m"    . python-nav-if-name-main)))

;;; Ruff
(use-package ruff-format
  :ensure t
  :hook (python-base-mode . ruff-format-on-save-mode))

(provide 'init-eglot)
