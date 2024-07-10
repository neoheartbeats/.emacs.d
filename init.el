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

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling
(setq fast-but-imprecise-scrolling t)

;; Introduced in Emacs 30, this inhibits fontification while
;; receiving input, which should help a little with scrolling performance
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame)
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes redisplay faster, but might produce incorrect
;; reordering of bidirectional text with embedded parentheses
(setq bidi-inhibit-bpa t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)            ; default is 0.5

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq-default read-process-output-max (* 1024 1024)) ; 1024kb

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage.
(setq inhibit-compacting-font-caches t)


;; Suppress GUI features
(setq use-dialog-box nil
      use-file-dialog nil)

(setq inhibit-splash-screen t
      inhibit-startup-buffer-menu t)

(setq initial-scratch-message "")

;; Default startup message
(defun display-startup-echo-area-message ()
  (let
      ((text "There's nothing more to Show"))
    (message "􀪾 %s" text)))


;; User information
(setq user-full-name "Sthenno"
      user-mail-address "sthenno@sthenno.com")


;; Package management via `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun straight-use-packages (packages)
  "Install multiple PACKAGES using `straight-use-package'."
  (dolist (pkg packages)
    (straight-use-package pkg)))

;; The `use-package' macro
(straight-use-packages '(use-package bind-key diminish))

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.1)
  (require 'cl-lib)
  (require 'use-package))


;; GCMH: the Garbage Collector Magic Hack
(use-package gcmh
  :straight t
  :diminish (gcmh-mode)
  :init

  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto           ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024)) ; 128mb
  :config (gcmh-mode 1))


;;; Load path
;;
;; Fix PATH for macOS
(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))

;; Org mode
;; (let ((org-lisp-dir (expand-file-name "site-lisp/org/lisp/" user-emacs-directory)))
;;   (add-to-list 'load-path org-lisp-dir)
;;   (require 'org))

;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-site-lisp.el
;; (defun add-subdirs-to-load-path (parent-dir)
;;   "Add every non-hidden subdir of PARENT-DIR to `load-path'."
;;   (let ((default-directory parent-dir))
;;     (setq load-path
;;           (append
;;            (cl-remove-if-not
;;             #'file-directory-p
;;             (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
;;            load-path))))

;; ;; Add both site-lisp and its immediate subdirs to `load-path'
;; (let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
;;   (push site-lisp-dir load-path)
;;   (add-subdirs-to-load-path site-lisp-dir))

;; Dir for init-* files
(push (expand-file-name "lisp/" user-emacs-directory) load-path)

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed.")

;; Set path for custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; Require init-* files
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-org)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
