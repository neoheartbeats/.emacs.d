;;; early-init.el --- pre-initialisation config -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This configuration runs before the package system and GUI initialization.
;; It focuses on optimizing startup performance and setting up basic frame parameters.

;;; Code:

;; Temporarily maximize garbage collection limits during startup
(setq-default gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 1.0)
(add-hook 'after-init-hook #'(lambda ()
                               (setopt gc-cons-threshold (* 80 1024 1024)
                                       gc-cons-percentage 0.1)))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))



;; Suppress messages during initialization for cleaner startup
;; (setq-default inhibit-message t)
;; (add-hook 'before-init-hook #'(lambda ()
;;                                 (setq inhibit-message nil)))

;; Set default frame parameters for all frames
;; These settings create a clean, modern UI appearance
(setopt default-frame-alist '((menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (width . 150)
                              (height . 75)
                              ;; (alpha-background . 60)
                              (alpha . (85 . 85))
                              (title . "􀫥 它们没能得到答案，只能看到凋零的生命。")
                              (ns-transparent-titlebar . t)))
(setopt initial-frame-alist default-frame-alist)

(require 'package)
(setopt package-enable-at-startup t)

(provide 'early-init)
