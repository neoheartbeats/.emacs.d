;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file bootstraps the configuration.
;;
;; TODO: Credits
;;

;;; Code:

;; Speed up startup
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

;; Font compacting can be terribly expensive, especially for rendering icon fonts on
;; MS. Whether disabling it has a notable affect on Linux and macOS hasn't been
;; determined, but do it anyway, just in case. This increases memory usage.
;; (setq inhibit-compacting-font-caches t)


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Suppress GUI features
(setq use-dialog-box nil
      use-file-dialog nil)

(setq inhibit-splash-screen t
      inhibit-startup-buffer-menu t)

(setq initial-scratch-message nil)

;; User information
(setq user-full-name "Sthenno"
      user-mail-address "sthenno@sthenno.com")

;; Set path for custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Emacs packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") 'prepend)

;; GCMH: the Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :demand t
  :config

  ;; The GC introduces annoying pauses and stuttering into our Emacs experience, so we
  ;; use `gcmh' to stave off the GC while we're using Emacs, and provoke it when it's
  ;; idle. However, if the idle delay is too long, we run the risk of runaway memory
  ;; usage in busy sessions. If it's too low, then we may as well not be using gcmh at
  ;; all.
  (defun gcmh-register-idle-gc ()
    "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
    (unless (eq this-command 'self-insert-command)
      (let ((idle-t (if (eq gcmh-idle-delay 'auto)
                        (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
                      gcmh-idle-delay)))
        (if (timerp gcmh-idle-timer)
            (timer-set-time gcmh-idle-timer idle-t)
          (setf gcmh-idle-timer
                (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))))
  (setq gcmh-idle-delay 'auto
        gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-verbose nil)
  (gcmh-mode 1))

;; Load path
(use-package org
  ;; :load-path "site-lisp/org/lisp/"
  :demand t)

;; Fix PATH for macOS
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))

;; Dir for init-* files
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
