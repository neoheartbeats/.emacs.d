;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file bootstraps the configuration.

;;; Code:

;;; Speed up startup

(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq gc-cons-percentage 0.5
                                        gc-cons-threshold (* 128 1024 1024))))

;; Garbage collect at the end of startup
(add-hook 'after-init-hook #'garbage-collect t)
(setq-default garbage-collection-messages nil)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 256 1024)) ; 256kb

;; Process performance tuning
(setq-default process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More preferment rapid scrolling over unfontified regions. May cause brief spells of
;; inaccurate fontification immediately after scrolling
(setq fast-but-imprecise-scrolling t)

;; Introduced in Emacs 30, this inhibits fontification while receiving input, which
;; should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; By default, Emacs updates its UI more often than it needs to
(setq idle-update-delay 1.0)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Don't pass case-insensitive to `auto-mode-alist'.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes re-display faster, but might produce incorrect reordering of
;; bidirectional text with embedded parentheses.
(setq bidi-inhibit-bpa t)

;; Font compacting can be very resource-intensive, especially when rendering icon fonts.
(setq inhibit-compacting-font-caches t)

;;; User information
(setq user-full-name "Sthenno"
      user-mail-address "sthenno@sthenno.com")

;;; Basic UI setup

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Suppress GUI features
(setq use-dialog-box nil
      use-file-dialog nil)

(setq-default inhibit-startup-screen t  ; This is not enough
              inhibit-startup-message t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-buffer-menu t)

(advice-add #'display-startup-screen :override #'ignore) ; This is enough

(setq inhibit-x-resources t)

(setq initial-buffer-choice nil
      initial-scratch-message nil)

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)


;;; Set path for custom-file
(setq custom-file (make-temp-file "_foo"))

;;; Emacs packages
(require 'package)

(use-package org :load-path "site-lisp/org/lisp/")

;; Add package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))

(use-package diminish
  :ensure t
  :demand t
  :config (diminish 'eldoc-mode))

;;; Fix PATH for macOS

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :demand t
;;   :config (exec-path-from-shell-initialize))

;;; Dir for init-* files
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; Require init-* files
(require 'init-system)
(require 'init-gui-frames)
(require 'init-org)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)

;;; _
(provide 'init)
