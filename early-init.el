;;; early-init.el --- Early initialization settings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tune startup behavior before the package system and regular init file run.

;;; Code:

;;; Temporarily maximize garbage collection limits during startup
(setq-default gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 1.0)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 1024 1024)
                                     gc-cons-percentage 0.1)))

;;; Emacs 31 prepares and activates `user-lisp-directory' automatically
(setq-default user-lisp-directory (locate-user-emacs-file "user-lisp/")
              user-lisp-auto-scrape t)

;;; Suppress messages during initialization for cleaner startup
(setq-default inhibit-message t)
(add-hook 'after-init-hook #'(lambda ()
                               (setq inhibit-message nil)))

;;; Set default frame parameters for all frames
(setq-default default-frame-alist '((tool-bar-lines . 0)
                                    (vertical-scroll-bars)
                                    (horizontal-scroll-bars)
                                    (width . 120)
                                    (height . 55)
                                    (internal-border-width . 0)
                                    (left-fringe . 0)
                                    (right-fringe .0)
                                    (scroll-bar-width . 1)
                                    (undecorated . t)
                                    (alpha-background . 0.6)
                                    (ns-alpha-elements . (ns-alpha-default
                                                          ns-alpha-fringe
                                                          ns-alpha-box
                                                          ns-alpha-stipple
                                                          ns-alpha-relief
                                                          ns-alpha-glyphs))))
(setq-default initial-frame-alist default-frame-alist)
(setq-default frame-resize-pixelwise t)

;;; Keep package activation enabled for installed packages
(setq-default package-enable-at-startup t)

(provide 'early-init)
