;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file bootstraps the configuration.
;;

;;; Code:

;; Speed up startup
(setq-default bidi-display-reordering 'left-to-right)

(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows
(setq-default cursor-in-non-selected-windows nil
	      highlight-nonselected-windows nil)

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

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Suppress GUI features
(setq use-dialog-box nil)
(setq use-file-dialog nil)

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

;; The `use-package' macro
(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(straight-use-package 'diminish)

;; (diminish 'eldoc-mode)

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-minimum-reported-time 0.01
        use-package-enable-imenu-support t)
  (require 'cl-lib)
  (require 'use-package))

;; Load path
(use-package emacs
  :config
  (defun dir-concat (dir file)
    "Join path DIR with filename FILE correctly."
    (concat (file-name-as-directory dir) file))

  ;; Add ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "site-lisp/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)

  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed."))

;; Set path for custom-file
(setq custom-file (locate-user-emacs-file "custom.el"))


;; GCMH
(use-package gcmh
  :straight t
  :diminish (gcmh-mode)
  :config
  (setq gcmh-high-cons-threshold (* 512 1024 1024))
  (gcmh-mode 1))

(setq jit-lock-defer-time 0)


;; Load components
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-org)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
