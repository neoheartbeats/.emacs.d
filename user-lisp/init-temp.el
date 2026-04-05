;;; init-temp.el --- Template helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains snippet and template helpers.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :config
  (setopt yas-triggers-in-field t)
  (yas-global-mode 1))

(provide 'init-temp)
