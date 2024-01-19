;;; init-comp.el --- Modern template system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; TempEl: Simple templating system
;;
(use-package tempel
  :straight t
  :bind (:map tempel-map
              ("<tab>" . tempel-next)
              ("<return>" . tempel-next)
              ("s-<tab>" . tempel-previous)
              ("<escape>" . tempel-done))
  :hook
  (prog-mode . tempel-abbrev-mode)
  (org-mode . tempel-abbrev-mode)
  :init (setq tempel-path "~/.emacs.d/temp.eld"))

;;;
;; To access the `.emacs.d/temp.eld' file
(defun open-emacs-temp-file ()
  "Open `temp.eld' file for TempEl."
  (interactive)
  (find-file "~/.emacs.d/temp.eld"))

(bind-keys :map global-map
           ("<f5>" . open-emacs-temp-file))

(provide 'init-temp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;

