;;; init-comp.el --- Modern template system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;; Yasnippet
(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode)
  :config
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)
  (yas-global-mode 1))

(provide 'init-temp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;

