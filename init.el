;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file bootstraps the configuration.
;;

;;; Code:

;;; Speed up startup
;;
;; Process performance tuning
(setq-default process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More preferment rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling
(setq fast-but-imprecise-scrolling t)

;; Introduced in Emacs 30, this inhibits fontification while
;; receiving input, which should help a little with scrolling performance
(setq redisplay-skip-fontification-on-input t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes re-display faster, but might produce incorrect
;; reordering of bidirectional text with embedded parentheses
(setq bidi-inhibit-bpa t)

;; Emacs "updates" its UI more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)            ; default is 0.5

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq-default read-process-output-max (* 1024 1024)) ; 1024kb

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage.
(setq inhibit-compacting-font-caches t)


(setq tool-bar-mode   nil
      menu-bar-mode   nil
      scroll-bar-mode nil)

(push '(width  . 120) default-frame-alist)
(push '(height . 50)  default-frame-alist)

;; NOTE: Since feature `alpha-background' conflicts with Emacs overlay rendering for
;; images such as SVG files, `alpha' can be a replacement for a rough approach.
;;
;; (push '(alpha . (90 . 90))           default-frame-alist)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark)        default-frame-alist)

;; Suppress GUI features
(setq use-dialog-box  nil
      use-file-dialog nil)

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t)

(setq initial-scratch-message nil)

;; User information
(setq user-full-name    "Sthenno"
      user-mail-address "sthenno@sthenno.com")

;; Caching
(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by Emacs are placed.")

;; Set path for custom-file
(setq custom-file (expand-file-name "custom.el" user-cache-directory))
(cond ((file-exists-p custom-file)
       (load custom-file)))


;; Emacs packages
;;
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("nongnu"    . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")))

;; Highest number gets priority
(setq package-archive-priorities '(("gnu-devel" . 4)
                                   ("gnu"       . 3)
                                   ("melpa"     . 2)
                                   ("nongnu"    . 1)))

;; Ensure to native-compile packages
(setq package-native-compile t)
(setq package-quickstart t)

;; The `use-package' macro
(setq use-package-enable-imenu-support t)


;; GCMH: the Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :defer 2
  :init

  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto           ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 512 1024 1024)) ; 512mb
  :config (gcmh-mode 1))


;; Load path
;;

;; Fix PATH for macOS
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config (exec-path-from-shell-initialize))

;; Dir for init-* files
(push (expand-file-name "lisp/" user-emacs-directory) load-path)

;; Require init-* files
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-org)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
