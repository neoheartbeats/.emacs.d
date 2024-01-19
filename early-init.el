;;; early-init.el --- Emacs 29+ pre-initialisation config -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Code loaded before the package system and GUI is initialized.
;;

;;; Code:
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;;
;; Customize Native-Compilation
;;

;; To maximize the speed of native compilation
(setq native-comp-speed 3)
(setq native-comp-async-report-warnings-errors 'silent)

;; To ensure `.eln' files are created correctly
(setq native-comp-async-query-on-exit t)

;; Do not enable packages during this early stage
(setq package-enable-at-startup nil)

;; Perform darwing the frame when initialization
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;;
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(alpha . (85 . 85)) default-frame-alist)

(push '(width . 145) default-frame-alist)
(push '(height . 40) default-frame-alist)

;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
