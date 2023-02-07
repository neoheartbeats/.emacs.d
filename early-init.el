;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:

;; Code loaded before the package system and GUI is initialized.

;;; Code:


;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

;; Pixelwise resize windows
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Faster to disable these here (before they've been initialized)
(push '(width . 150) default-frame-alist)
(push '(height . 75) default-frame-alist)
(push '(alpha . (90 . 90)) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;; Prevent Emacs making packages at startup
(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for GccEmacs
(setq inhibit-automatic-native-compilation t)
(setq native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)

;; Suppress GUI features
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(setq inhibit-default-init t)
(setq inhibit-compacting-font-caches t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq idle-update-delay 1.0)


(provide 'early-init)
;;; early-init.el ends here
