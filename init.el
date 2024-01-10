;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Sthenno

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

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows
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

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Suppress GUI features
(setq use-dialog-box nil)
(setq use-file-dialog nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message "")

;; Default startup message
(defun display-startup-echo-area-message ()
  (let
      ((text "贵安! 游荡不定的黑之魂."))
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

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-minimum-reported-time 0.01
        use-package-enable-imenu-support t)
  (require 'cl-lib)
  (require 'use-package))


;;
;; If `PATH' is not configured while compiling
;;
;; (use-package exec-path-from-shell
;;   :straight t
;;   :config (exec-path-from-shell-initialize))

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

;;
;; GCMH
;;
(use-package gcmh
  :defer 2
  :straight t
  :config
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
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-verbose nil)
  (gcmh-mode 1))

;;
;; Org Mode
;;
(use-package org
  :defer
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
