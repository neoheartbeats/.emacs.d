;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration.
;; It handles core settings, performance optimizations, and loads module-specific
;; configurations from the lisp/ directory.

;;; Code:

;;; Performance Optimizations

;; Increase memory threshold for GC during init
(let ((default-gc-cons-percentage gc-cons-percentage))
  (setq gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (setq gc-cons-percentage default-gc-cons-percentage)

                                    ;; Collect garbage after init
                                    (garbage-collect))
            t))

;; Silence GC messages
(setq garbage-collection-messages nil)

;; Process & I/O Optimizations
(setq read-process-output-max (* 1024 1024) ; Increase to 1mb
      process-adaptive-read-buffering nil   ; Disable adaptive buffering
      inhibit-compacting-font-caches t      ; Don't compact font caches during GC
      load-prefer-newer t)                  ; Prefer newer elisp files

;; Display Engine Optimizations
(setq redisplay-skip-fontification-on-input t ; Skip fontification during input
      fast-but-imprecise-scrolling t          ; Faster scrolling
      idle-update-delay 1.0                   ; Reduce idle display updates
      frame-inhibit-implied-resize t          ; Disable frame resizing
      inhibit-message nil)                    ; Allow messages during init

;; Better Directory Handling
(setq auto-mode-case-fold nil           ; Case-sensitive `auto-mode-alist' lookup
      find-file-visit-truename nil      ; Don't resolve symlinks
      vc-follow-symlinks t)             ; Follow symlinks for version control

;; Bidirectional Text Handling
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Basic startup settings
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;;; User Configuration
(setq user-full-name "Sthenno"
      user-mail-address "sthenno@sthenno.com")

;;; UI Configuration
;; Disable unnecessary UI elements
(dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Custom Startup message
(define-advice display-startup-echo-area-message (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((icon "􂨖")
        (text "Left to Bloom, Bloom to Death"))
    (message "%s %s" icon text)))

;;; Package Management
;; Store customizations in temporary file
(setq custom-file (make-temp-file "custom-tmp"))

;; Initialize package system
(require 'package)

(setq package-archives
      '(("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;;; Core Package Configuration

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)

  ;; Configure use-package defaults
  (setq use-package-expand-minimally t       ; Generate minimal code
        use-package-enable-imenu-support t)) ; Better imenu integration

;; Essential packages

(use-package org
  :load-path "site-lisp/org/lisp/")

(use-package diminish
  :ensure t
  :demand t
  :config
  (diminish 'eldoc-mode))

;;; Load Configuration Modules
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; Define required modules
(defvar sthenno/init-modules
  '(init-system
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
