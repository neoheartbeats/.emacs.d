;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:

;; Code loaded before the package system and GUI is initialized.

;;; Code:


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;; Enhance the performance when font is not configured as system default
(setq frame-inhibit-implied-resize t)


;; Prevent `package.el' loading packages prior to their init-file loading
;; This procedure is related to
;; https://github.com/radian-software/straight.el/
(setq package-enable-at-startup nil)


;; Config related to GccEmacs
;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil)

;; Compile external packages for GccEmacs
(setq native-comp-async-report-warnings-errors nil)
(setq package-native-compile t)


(provide 'early-init)
;;; early-init.el ends here
