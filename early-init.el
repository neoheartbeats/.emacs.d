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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A big contributor to startup times is garbage collection
;;
;; https://github.com/casouri/lunarymacs/blob
;; /6ce1a6da38d5e5c261d71a495ee2fdbd051303f9/early-init.el#L3-L26
;;
;; Defer garbage collection further back in the startup process
(add-hook 'emacs-startup-hook
  (let (
				 (old-list file-name-handler-alist)
         (threshold (* 100 gc-cons-threshold))
         (percentage gc-cons-percentage))
    (lambda ()
      (setq file-name-handler-alist old-list)
      (setq gc-cons-threshold threshold)
      (setq gc-cons-percentage percentage)
      (garbage-collect))) t)

(setq package-enable-at-startup nil)
(setq file-name-handler-alist nil)
(setq message-log-max 16384)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq auto-window-vscroll nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compile external packages for GccEmacs
(setq package-native-compile t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prevent `package.el' loading packages prior to init-file loading
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keep it quiet
(setq warning-minimum-level :error)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Scratch Buffer settings
(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq pop-up-windows nil)
(setq save-silently t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the frame parameters before it's drawing
(setq default-frame-alist
  '(
		 (top . 115)
     (left . 320)
	   (width . 105)
	   (height . 45)
     (alpha . (90 . 65))))

;; Make UTF-8 the default coding system
(set-language-environment "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default lexical binding
(setq-default lexical-binding t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS key mapping
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
