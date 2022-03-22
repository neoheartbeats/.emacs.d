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

;; Prevent unwanted runtime compilation for gccemacs
(setq native-comp-deferred-compilation nil)

;; Prevent `package.el' loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;; Make UTF-8 the default coding system
(set-language-environment "UTF-8")
