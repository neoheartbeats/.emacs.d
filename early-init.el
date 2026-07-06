;;; early-init.el --- Early initialization settings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tune startup behavior before the package system and regular init file run.

;;; Code:

;;; Loading and packages
(defvar sthenno/gc-cons-threshold (* 64 1024 1024)
  "Garbage collection threshold restored after startup.")

(defvar sthenno/file-name-handler-alist file-name-handler-alist
  "File name handlers restored after startup.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      load-prefer-newer t
      load-path-filter-function #'load-path-filter-cache-directory-files
      native-comp-async-report-warnings-errors 'silent
      package-enable-at-startup nil
      package-quickstart nil
      read-process-output-max (* 4 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold sthenno/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist sthenno/file-name-handler-alist)))

;;; Frames
(setq frame-resize-pixelwise t
      default-frame-alist '(
                            ;; (menu-bar-lines . 0) (tool-bar-lines . 0)
                            (vertical-scroll-bars) (horizontal-scroll-bars)
                            (height . 45) (width . 120)
                            (background-color . "000000")
                            (undecorated . t)
                            (ns-alpha-elements . ns-alpha-all)
                            (alpha . (85 . 85)))
      initial-frame-alist default-frame-alist)

;; (add-to-list 'default-frame-alist '(alpha-background . 0.55))
;; (add-to-list 'default-frame-alist '(ns-background-blur . 15))
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;; early-init.el ends here
