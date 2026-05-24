;;; early-init.el --- Early initialization settings -*- lexical-binding: t; no-byte-compile: t -*-

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
(require 'cl-lib)

;;; Loading
(setq load-prefer-newer t)
(setq load-path-filter-function #'load-path-filter-cache-directory-files)

(setq package-install-upgrade-built-in t
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-selected-packages '(auctex consult corfu denote denote-journal denote-org
                                         gptel magit marginalia ultra-scroll vertico yasnippet))

;;; Encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)

;;; Frames
(setq frame-resize-pixelwise t)
(setq-default default-frame-alist '((menu-bar-lines . 0) (tool-bar-lines . 0)
                                    (vertical-scroll-bars) (horizontal-scroll-bars)
                                    (height . 45) (width . 120)
                                    (top . 0.5) (left . 0.5)
                                    (undecorated . t)
                                    (alpha-background . 0.60) (ns-background-blur . 20)
                                    (ns-alpha-elements . (ns-alpha-default
                                                          ns-alpha-fringe
                                                          ns-alpha-box
                                                          ns-alpha-stipple
                                                          ns-alpha-relief
                                                          ns-alpha-glyphs))))

(setq-default initial-frame-alist default-frame-alist)

(provide 'early-init)
