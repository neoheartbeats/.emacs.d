;;; early-init.el --- Early initialization settings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tune startup behavior before the package system and regular init file run.

;;; Code:

;; Temporarily maximize garbage collection limits during startup
(setq-default gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 1.0)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 80 1024 1024)
                                   gc-cons-percentage 0.1)))

;; Emacs 31 prepares and activates `user-lisp-directory' automatically
(setq-default user-lisp-directory (locate-user-emacs-file "user-lisp/")
              user-lisp-auto-scrape t)

;; Suppress messages during initialization for cleaner startup
(setq-default inhibit-message t)
(add-hook 'after-init-hook (lambda ()
                             (setq inhibit-message nil)))

;; Set default frame parameters for all frames
(setq-default default-frame-alist '((menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (vertical-scroll-bars . nil)
                                    (horizontal-scroll-bars . nil)
                                    (width . 135)
                                    (height . 65)
                                    (alpha . (90 . 90))
                                    (title . "")
                                    (ns-transparent-titlebar . t)))
(setq-default initial-frame-alist default-frame-alist)

;; Keep package activation enabled for installed packages
(setq package-enable-at-startup t)
