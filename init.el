;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file bootstraps the configuration.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Speed up startup
;;
;; Defer garbage collection further back in the startup process
;;
;; The garbage-collect system may be controlled by `gcmh'
(setq gc-cons-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To enhance the core performance
;;
;; Disable bidirectional text rendering for a modest performance boost. Just
;; need to remember to turn it on when displaying a right-to-left language!
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize packages
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(eval-when-compile ; Required by `use-package'
  (setq use-package-verbose nil)
  (setq use-package-compute-statistics nil)
  (setq use-package-minimum-reported-time 0.01)
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;; Packages bundled with `use-package'
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage Collector Magic Hack
(use-package gcmh :ensure t
  :defer 2
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold most-positive-fixnum)
  (gcmh-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap process
;;
;; Load path
;;
;; Force `lisp' at head to reduce the startup cost
(defun my-update-load-path (&rest _)
  "Update `load-path'."
  (dolist (subdirs '("lisp/"))
    (push (expand-file-name subdirs user-emacs-directory) load-path)))
(my-update-load-path)

;; Setup `custom.el'
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Call the function to setup Org Mode
(use-package org :load-path "site-lisp/org-lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load components
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-comp)
(require 'init-temp)
(require 'init-org)
(require 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init.el ends here

