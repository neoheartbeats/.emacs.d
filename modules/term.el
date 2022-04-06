;; term.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Emacs-libvterm setup.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs-libvterm
(use-package vterm
  :bind
  (
		("M-`" . vterm-toggle)
		(:map vterm-mode-map
			("s-k" . vterm-clear)
			("C-c" . vterm-send-C-c)))
  :custom (vterm-always-compile-module t)
  :init
  (defun vterm-split-window-below ()
    (interactive)
    (vterm)
    (split-window-below -12)
    (previous-buffer)
    (other-window 1))
  (defun vterm-toggle ()
    "Toggle vterm open and hide with 'M-`'"
    (interactive)
    (if (eq major-mode 'vterm-mode)
      (delete-window)
      (vterm-split-window-below)))
  :config (setq kill-buffer-query-functions nil))

(provide 'term)
