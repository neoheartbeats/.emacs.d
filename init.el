;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file bootstraps the configuration.
;;
;; TODO: Credits
;;

;;; Code:

;;; Speed up startup
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

;; Garbage collect at the end of startup
(add-hook 'after-init-hook #'garbage-collect t)

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

;;; Basic UI setup
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Suppress GUI features
(setq use-dialog-box nil
      use-file-dialog nil)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message "Sthenno")

(setq initial-buffer-choice t
      initial-scratch-message "")

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;;; User information
(setq user-full-name "Sthenno"
      user-mail-address "sthenno@sthenno.com")

;;; Set path for custom-file (do not ask me about this for any reason)
(setq custom-file (make-temp-file "_tmp"))

(use-package org
  :load-path "site-lisp/org/lisp/"
  :demand t
  :init
  (require 'org)
  (require 'org-latex-preview))

;;; Emacs packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") 'prepend)

(use-package diminish
  :ensure t
  :config (diminish 'eldoc-mode))

;;; Dir for init-* files
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Require init-* files
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-org)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)

(provide 'init)
