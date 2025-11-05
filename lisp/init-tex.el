;;; init-tex.el --- TeX Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . turn-on-reftex))
  :config
  (setopt TeX-auto-save t)
  (setopt TeX-parse-self t)
  (setopt TeX-save-query nil)
  (setopt TeX-source-correlate-start-server t)
  (setopt TeX-view-program-selection '((output-pdf "PDF Tools")))
  )


(provide 'init-tex)
