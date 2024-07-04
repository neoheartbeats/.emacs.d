;;; early-init.el --- Emacs 29+ pre-initialisation config -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Code loaded before the package system and GUI is initialized.
;;

;;; Code:
;;

;;; [FIXME]
(setenv "LIBRARY_PATH" (concat "/opt/homebrew/opt/gcc/lib/gcc/14:"
                               "/opt/homebrew/opt/libgccjit/lib/gcc/14:"
                               "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14"))

;;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;;; Customize Native-Compilation
;;
;; To maximize the speed of native compilation
(setq native-comp-speed 3)
(setq native-comp-async-report-warnings-errors 'silent)

;; To ensure `.eln' files are created correctly
(setq native-comp-async-query-on-exit t)
(setq native-compile-prune-cache t)

;; Do not enable packages during this early stage
(setq package-enable-at-startup nil)

;;; Perform darwing the frame when initialization
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(width . 120) default-frame-alist)
(push '(height . 50) default-frame-alist)

;; NS
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
;; (push '(alpha-background . 80) default-frame-alist)

;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
