;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file bootstraps the configuration.
;;

;;; Code:

;;
;; Speed up startup
;;
(setq-default bidi-display-reordering 'left-to-right)

(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows
(setopt cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setopt fast-but-imprecise-scrolling t)
(setopt redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setopt frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setopt ffap-machine-p-known 'reject)

;; Don't pass case-insensitive to `auto-mode-alist'
(setopt auto-mode-case-fold nil)

;; Suppress GUI features
(setopt use-dialog-box nil)
(setopt use-file-dialog nil)

(setopt inhibit-splash-screen t)
(setopt inhibit-startup-buffer-menu t)
(setopt initial-scratch-message "")

;; Default startup message
(defun display-startup-echo-area-message ()
  (let
      ((text "There's nothing more to show."))
    (message "􀪾 %s" text)))

;;
;; Package management via `straight.el'
;;
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

  ;; Adds ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "site-lisp/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  
  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed."))

;; Set path for custom-file
(setopt custom-file (locate-user-emacs-file "custom.el"))

;; GCMH
(use-package gcmh
  :straight t
  :diminish (gcmh-mode)
  :config
  (setopt gcmh-high-cons-threshold (* 512 1024 1024))
  (gcmh-mode 1))

(setopt jit-lock-defer-time 0)

;;
;; Org Mode
;;
(use-package org
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

;;
;; Load components
;;
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-org)
(require 'init-eglot)

;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
