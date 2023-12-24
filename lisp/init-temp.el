;;; init-comp.el --- Modern template system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 KAMUSUSANOWO

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; TempEl: Simple templating system
;;
(use-package tempel
  :straight t
  :bind
  (("s-." . tempel-expand)
   ("s-/" . tempel-insert))
  :init (setq tempel-path "~/.emacs.d/temp.eld"))

(provide 'init-temp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;

