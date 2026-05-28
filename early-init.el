;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tune startup behavior before the package system and regular init file run.

;;; Code:

;;; Loading and packages
(setq load-prefer-newer t
      load-path-filter-function #'load-path-filter-cache-directory-files
      package-install-upgrade-built-in t
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-selected-packages '(auctex consult corfu denote denote-journal denote-org
                                         gptel magit marginalia ultra-scroll vertico))

;;; Frames
(setq-default frame-resize-pixelwise t
              default-frame-alist '((menu-bar-lines . 0) (tool-bar-lines . 0)
                                    (vertical-scroll-bars) (horizontal-scroll-bars)
                                    (height . 45) (width . 120)
                                    (undecorated . t))
              initial-frame-alist default-frame-alist)

(provide 'early-init)
