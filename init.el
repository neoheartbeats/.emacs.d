;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration.
;; It handles core settings, performance optimizations, and loads module-specific
;; configurations from the lisp/ directory.

;;; Code:
;;

;;; Large File Handling - Performance Optimizations

;; Process & I/O Optimizations
;;
;; Increase read buffer to 4MB for better I/O performance
(setq read-process-output-max (* 4 1024 1024))

;; Set warning threshold to 512MB before prompting about large files
(setq large-file-warning-threshold (* 512 1024 1024))

;; Disable adaptive buffering for more predictable performance
(setq process-adaptive-read-buffering nil)

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

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Remove command line options that aren't relevant to our current OS
(setq command-line-x-option-alist nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows
;;
(setq-default cursor-in-non-selected-windows nil)
(setq-default highlight-nonselected-windows nil)
(setq-default ns-use-proxy-icon nil)

;; Better Directory Handling
(setq auto-mode-case-fold nil)          ; Case-sensitive `auto-mode-alist' lookup
(setq find-file-visit-truename nil)     ; Don't resolve symlinks
(setq vc-follow-symlinks t)             ; Follow symlinks for version control

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
(dolist (mode '(scroll-bar-mode menu-bar-mode tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Custom Startup message
(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((icon (propertize "􀎛" 'face 'default))
        (text "循此苦旅，終抵群星。"))
    (message "%s %s" icon text)))

;;; Package Management
(setq package-vc-allow-build-commands t
      package-vc-register-as-project nil
      package-install-upgrade-built-in t
      package-archives '(("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("nongnu" . 100)
                                   ("gnu" . 100)
                                   ("gnu-devel" . 80)
                                   ("melpa" . 60)
                                   ("melpa" . 0)))

(use-package persistent-cached-load-filter
  :ensure t
  :vc ( :url "https://github.com/include-yy/persistent-cached-load-filter"
        :branch "master"
        :rev
        :newest)
  :config (persistent-cached-load-filter-easy-setup))

;; Append PATH from shell
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init (when (memq window-system '(mac ns x))
;;           (exec-path-from-shell-initialize)))

;; Load the patched `org'
(progn
  (add-to-list 'load-path "/Users/sthenno/.emacs.d/site-lisp/org/lisp/")
  (setq features (delq 'org features))
  (require 'org))

;; Declare interactive functions used at startup to inform the byte-compiler
(let ((startup-buffer 'denote-journal-new-or-existing-entry))
  (declare-function startup-buffer "denote-journal" t)
  (setq initial-buffer-choice startup-buffer))

;; Store customizations
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; Load configuration modules
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'init-system)
(require 'init-gui-frames)
(require 'init-org)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)

(provide 'init)

;;; init.el ends here
