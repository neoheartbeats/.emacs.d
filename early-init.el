;;; early-init.el --- pre-initialisation config -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This configuration runs before the package system and GUI initialization.
;; It focuses on optimizing startup performance and setting up basic frame parameters.

;; Key components:
;; - Garbage collection optimization for startup
;; - File handler management for faster loading
;; - Native compilation configuration
;; - Initial frame parameter settings
;; - Basic UI element suppression

;;; Code:

;; Temporarily maximize garbage collection limits during startup
;; These values are restored to more reasonable defaults after initialization
(let ((threshold (* 128 1024 1024))     ; 128MB
      (percentage 0.6))
  (setq-default gc-cons-threshold (* 4 1024 1024 1024) ; Set to 4GB
                gc-cons-percentage 1.0)
  (add-hook 'after-init-hook #'(lambda ()
                                 (setq gc-cons-threshold threshold
                                       gc-cons-percentage percentage))))

;; Suppress messages during initialization for cleaner startup
(setq-default inhibit-message t)
(add-hook 'after-init-hook #'(lambda ()
                               (setq inhibit-message nil)))

;; Reset frame parameters for clean slate
(setq default-frame-alist nil)

;; Temporarily disable file-name-handler-alist during startup for faster loading
;; This significantly speeds up file operations during initialization
(when (and (not noninteractive)
           (not (daemonp)))
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'after-init-hook
              #'(lambda ()
                  (setq file-name-handler-alist
                        (delete-dups (append file-name-handler-alist
                                             old-file-name-handler-alist)))))))

;; Suppress loading messages during startup to prevent screen flashing
(define-advice load-file (:override (file) silence-loading)
  "Override `load-file' to suppress loading messages during startup."
  (load file nil 'nomessage))

(define-advice startup--load-user-init-file (:before (&rest _) restore-load-file)
  "Restore original `load-file' behavior before loading user init file.
This ensures normal message behavior after startup is complete."
  (advice-remove #'load-file #'load-file@silence-loading))

;; Define user cache directory for various Emacs temporary files
(defcustom user-cache-directory (expand-file-name "~/.cache/emacs/")
  "Base directory for Emacs cache files.
This directory is used for native compilation cache, auto-save files,
and other temporary data that Emacs generates during operation."
  :type 'directory
  :group 'initialization)

;; Ensure cache directory exists to prevent runtime errors
;; (make-directory user-cache-directory t)

;; Configure native compilation settings when available

;; Suppress native compilation warnings to prevent disruption
(setq native-comp-async-report-warnings-errors 'silent)

;; Set optimal number of compilation jobs based on available processors
(setq native-comp-async-jobs-number (max 1
                                         (- (num-processors) 2)))

;; Set default frame parameters for all frames
;; These settings create a clean, modern UI appearance
(setq-default default-frame-alist '(
                                    ;; Disable UI elements for a minimal look
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (vertical-scroll-bars . nil)
                                    (horizontal-scroll-bars . nil)

                                    ;; Set default frame size
                                    (width . 125)
                                    (height . 60)

                                    ;; (alpha-background . 60)
                                    (alpha . (90 . 90))

                                    ;; macOS-specific titlebar settings
                                    (ns-transparent-titlebar . t)))

(setq-default initial-frame-alist default-frame-alist)

;; Do `package-initialize' at `init'
(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
