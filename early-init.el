;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:

;; Code loaded before the package system and GUI is initialized.

;;; Code:

;;; Pixelwise resize windows
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1920)))
  (add-to-list var '(height . (text-pixels . 990)))
  (add-to-list var '(alpha . (90 . 90))))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . light) default-frame-alist)


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)


;; Prevent `package.el' loading packages prior to their init-file loading
;; This procedure is related to
;; https://github.com/radian-software/straight.el/
(setq package-enable-at-startup nil)


;; Config related to GccEmacs
;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(setq package-quickstart t)

;; Compile external packages for GccEmacs
(setq package-native-compile t)

;; Suppress GUI features
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(setq inhibit-default-init t)
(setq native-comp-async-report-warnings-errors 'silent)


;;; Byte compiled files
(setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))


(provide 'early-init)
;;; early-init.el ends here
