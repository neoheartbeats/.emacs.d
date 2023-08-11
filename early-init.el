;; early-init.el --- Emacs 29+ pre-initialisation config
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code loaded before the package system and GUI is initialized.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance darwing the frame when initialization
;;
;; Faster to disable these here (before they've been initialized)
(push '(width . 165) default-frame-alist)
(push '(height . 75) default-frame-alist)
(push '(alpha . 0.9) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Suppress GUI features
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(setq inhibit-default-init t)
(setq native-comp-async-report-warnings-errors 'silent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package or GccEmacs related settings
;;
;; Prevent Emacs making packages at startup
(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil) ; obsolete since 29.1
(setq native-comp-jit-compilation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; early-init.el ends here
