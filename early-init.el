;; early-init.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code loaded before the package system and GUI is initialized.
;;
;; Code:

;; A big contributor to startup times is garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil)

;; Prevent `package.el' loading packages prior to their init-file loading
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; Keep it quiet
(setq warning-minimum-level :error)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq pop-up-windows nil)

;; Set the frame parameters before it's drawing
(setq default-frame-alist
      '((top . 45)
        (left . 160)
	    (width . 150)
	    (height . 42)))

;; Make UTF-8 the default coding system
(set-language-environment "UTF-8")
