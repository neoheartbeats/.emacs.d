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
(setopt read-process-output-max (* 4 1024 1024))

;; Set warning threshold to 512MB before prompting about large files
(setopt large-file-warning-threshold (* 512 1024 1024))

;; File management settings
;;
;; Disable automatic file saving to prevent I/O spikes
(setopt auto-save-default nil)
(setopt save-silently t)

;; Disable creation of lock files to reduce file operations
(setopt create-lockfiles nil)

;; Disable backup file creation to reduce disk usage and I/O
(setopt make-backup-files nil)

;; Display Engine Optimizations
;;
;; Skip fontification during input to improve input responsiveness
(setopt redisplay-skip-fontification-on-input t)

;; Enable faster scrolling by allowing minor visual inaccuracies
(setopt fast-but-imprecise-scrolling t)

;; Prevent automatic frame resizing for better performance
(setopt frame-inhibit-implied-resize t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows
;;
(setopt cursor-in-non-selected-windows nil)
(setopt ns-use-proxy-icon nil)

;; Better Directory Handling
(setopt auto-mode-case-fold nil)        ; Case-sensitive `auto-mode-alist' lookup
(setopt find-file-visit-truename nil)   ; Don't resolve symlinks
(setopt vc-follow-symlinks t)           ; Follow symlinks for version control

;; Bidirectional Text Handling
(setopt bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t)

;; Basic startup settings
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message nil)

;;; User configurations
(setopt user-full-name user-login-name
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
        (text "它们没能得到答案，只能看到凋零的生命。"))
    (message "%s %s" icon text)))

(setopt elisp-fontify-semantically t)

;;; Package Management
(setopt package-vc-allow-build-commands t
        package-install-upgrade-built-in t
        package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           )
        ;; package-archive-priorities '(("nongnu" . 100)
        ;;                              ("gnu" . 100)
        ;;                              ("gnu-devel" . 80)
        ;;                              ("melpa" . 60)
        ;;                              ("melpa" . 0))
        )

(require 'use-package)
(setopt use-package-enable-imenu-support t)
(setopt use-package-compute-statistics t)
(setopt use-package-vc-prefer-newest t)

;; (use-package persistent-cached-load-filter
;;   :ensure t
;;   :vc ( :url "https://github.com/include-yy/persistent-cached-load-filter"
;;         :branch "master"
;;         :rev
;;         :newest)
;;   :config (persistent-cached-load-filter-easy-setup))


;; Append PATH from shell
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init (when (memq window-system '(mac ns x))
;;           (exec-path-from-shell-initialize)))

;; Load the patched `org'
;; (progn
;;   (add-to-list 'load-path "/Users/sthenno/.emacs.d/site-lisp/org/lisp/")
;;   (setq features (delq 'org features))
;;   (require 'org))

;; Declare interactive functions used at startup to inform the byte-compiler
(let ((startup-buffer 'denote-journal-new-or-existing-entry))
  (declare-function startup-buffer "denote-journal" t)
  (setopt initial-buffer-choice startup-buffer))

;; Store customizations
(let ((fp (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p fp)
    (make-empty-file fp))
  (setopt custom-file fp)
  (load custom-file))


;; Load configuration modules
(require 'init-system)
(require 'init-gui-frames)
(require 'init-tex)
(require 'init-org)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
