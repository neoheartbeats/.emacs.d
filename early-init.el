;;; early-init.el --- Early initialization settings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tune startup behavior before the package system and regular init file run.

;;; Code:

;;; Startup
(setq-default gc-cons-threshold (* 512 1024 1024)
              gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq gc-cons-threshold (* 128 1024 1024)
                                        gc-cons-percentage 0.1)))

;;; Loading
(setq-default load-path-filter-function #'load-path-filter-cache-directory-files)

;;; Frames
(setq-default default-frame-alist
              '((tool-bar-lines . 0) (horizontal-scroll-bars) (vertical-scroll-bars)
                (height . 45) (width . 120)
                (internal-border-width . 0) (left-fringe . 0) (right-fringe . 0)
                (undecorated . t) (alpha-background . 0.65) (ns-background-blur . 20)
                (ns-alpha-elements . (ns-alpha-default ns-alpha-fringe ns-alpha-box ns-alpha-stipple
                                                       ns-alpha-relief ns-alpha-glyphs))))
(setq-default initial-frame-alist default-frame-alist)
(setq-default frame-resize-pixelwise t)

(provide 'early-init)
