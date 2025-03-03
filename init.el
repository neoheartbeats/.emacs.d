;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration.
;; It handles core settings, performance optimizations, and loads module-specific
;; configurations from the lisp/ directory.

;;; Code:


;;; Large File Handling - Performance Optimizations

;; Process & I/O Optimizations
;;
;; Increase read buffer to 4MB for better I/O performance
(setq read-process-output-max (* 4 1024 1024))

;; Set warning threshold to 512MB before prompting about large files
(setq large-file-warning-threshold (* 512 1024 1024))

;; Disable adaptive buffering for more predictable performance
(setq process-adaptive-read-buffering nil)

;; Prevent font cache compaction during GC for better responsiveness
(setq inhibit-compacting-font-caches t)

;; Always load newer elisp files to ensure using latest version
(setq load-prefer-newer t)

;; File management settings
;;
;; Disable automatic file saving to prevent I/O spikes
(setq auto-save-default nil)
(setq save-silently t)

;; Disable creation of lock files to reduce file operations
(setq create-lockfiles nil)

;; Disable backup file creation to reduce disk usage and I/O
(setq make-backup-files nil)

;; Display Engine Optimizations
;;
;; Skip fontification during input to improve input responsiveness
(setq redisplay-skip-fontification-on-input t)

;; Enable faster scrolling by allowing minor visual inaccuracies
(setq fast-but-imprecise-scrolling t)

;; Prevent automatic frame resizing for better performance
(setq frame-inhibit-implied-resize t)

;; Suppress messages during initialization for cleaner startup
(setq inhibit-message t)

;;; JIT (Just-In-Time) configuration - Performance Optimizations

(setq jit-lock-defer-time nil)          ; inhibit delay for high responsiveness
(setq jit-lock-chunk-size (* 128 1024)) ; Process larger chunks at once

;; Set background fontification parameters optimized for large files
;;
(setq jit-lock-stealth-time 2)       ; Start background processing after 2s of idle time
(setq jit-lock-stealth-nice 0.2)     ; Allocate more CPU time to background processing
(setq jit-lock-stealth-load 200)     ; Allow higher CPU load for background processing

(setq jit-lock-stealth-verbose nil)     ; Disable messages during stealth fontification

;; Adjust context-based fontification for performance
;;
(setq jit-lock-contextually t)          ; Enable contextual analysis
(setq jit-lock-context-time 0.5)        ; Context analysis during longer idle periods
(setq jit-lock-context-distance 5000)   ; Extended context distance for better accuracy

;; Enhance performance for large buffers by disabling syntax-based fontification
;;
(defun sthenno/adjust-jit-lock-for-large-files ()
  "Adjust JIT-lock parameters based on file size for optimal performance."
  (let ((size (buffer-size)))
    (cond
     ((> size (* 50 1024 1024))         ; 50MB
      (setq-local jit-lock-defer-time 0.5)
      (setq-local jit-lock-contextually nil)
      (setq-local font-lock-maximum-decoration 0))
     ((> size (* 20 1024 1024))         ; 20MB
      (setq-local jit-lock-defer-time 0.2)
      (setq-local jit-lock-chunk-size 5000)
      (setq-local font-lock-maximum-decoration 1))
     ((> size (* 5 1024 1024))          ; 5MB
      (setq-local jit-lock-defer-time 0.1)
      (setq-local font-lock-maximum-decoration 2))
     (t                                 ; Normal files
      (setq-local jit-lock-defer-time nil)
      (setq-local jit-lock-chunk-size (* 128 1024))
      (setq-local font-lock-maximum-decoration 3)))))
(add-hook 'find-file-hook #'sthenno/adjust-jit-lock-for-large-files)

;; Enable incremental syntax highlighting even during active editing
(setq jit-lock-defer-multiline t)

(setq jit-lock-antiblink-grace 0.5)     ; Reduce blinking during rapid scrolling

(when (fboundp 'jit-lock-register)
  (setq jit-lock-stealth-nice 0.1))     ; Lower allocates more CPU time


;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows
;;
(setq-default cursor-in-non-selected-windows nil)
(setq-default highlight-nonselected-windows nil)

(setq-default ns-use-proxy-icon nil)
(setq-default frame-title-format "")

;; Better Directory Handling
(setq auto-mode-case-fold nil)          ; Case-sensitive `auto-mode-alist' lookup
(setq find-file-visit-truename nil)     ; Don't resolve symlinks
(setq vc-follow-symlinks t)             ; Follow symlinks for version control

;; Suppress warnings "Package cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; Bidirectional Text Handling
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Basic startup settings
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-default-init t
      initial-scratch-message nil)


;;; User configurations

(setq user-full-name user-login-name
      user-mail-address "sthenno@sthenno.com")


;;; UI configurations

;; Disable unnecessary UI elements
(dolist (mode '(scroll-bar-mode menu-bar-mode)) ; tool-bar-mode
  (when (fboundp mode) (funcall mode -1)))

;; Custom Startup message
(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((icon "􂨖")
        (text "Left to Bloom, Bloom to Death"))
    (message "%s %s" icon text)))


;;; Package Management

;; Store customizations
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(cond ((file-exists-p custom-file)
       (load custom-file)))

;; Initialize package system
(require 'package)

(setq package-archives '(("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Core package configurations
;;
;; Essential packages
(use-package org :load-path "site-lisp/org/lisp/" :demand t)
(use-package diminish :ensure t :demand t)

;; Load configuration modules
;;
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; Define required modules
(defvar sthenno/init-modules '(init-system
                               init-gui-frames
                               init-org
                               init-editing-utils
                               init-projects
                               init-temp
                               init-comp
                               init-eglot)
  "List of configuration modules to load.")

;; Load modules safely
(dolist (module sthenno/init-modules)
  (condition-case err
      (require module)
    (error
     (message "Failed to load module \"%s\": %s" module err))))

(provide 'init)

;;; init.el ends here
