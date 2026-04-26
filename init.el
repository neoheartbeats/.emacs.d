;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration. It handles
;; core settings, performance optimizations, and loads module-specific configurations
;; from `user-lisp/'.

;;; Code:

;;; Process and display performance
(setq-default read-process-output-max (* 4 1024 1024)
              large-file-warning-threshold (* 512 1024 1024)
              auto-save-default nil
              save-silently t
              create-lockfiles nil
              make-backup-files nil
              redisplay-skip-fontification-on-input t
              fast-but-imprecise-scrolling t
              frame-inhibit-implied-resize t
              cursor-in-non-selected-windows nil
              ns-use-proxy-icon nil
              auto-mode-case-fold nil
              find-file-visit-truename nil
              vc-follow-symlinks t
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;;; Basic startup settings
(setq-default inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-buffer-menu t
              inhibit-default-init t
              initial-scratch-message nil)

(setq-default user-full-name user-login-name
              user-mail-address "sthenno@sthenno.com"
              elisp-fontify-semantically t)

;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
(tool-bar-mode -1)

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((icon (propertize "􀎛" 'face 'default))
        (text "它们没能得到答案，只能看到凋零的生命。"))
    (minibuffer-message "%s %s" icon text)))

;;; Package management
(setq-default package-native-compile t
              package-install-upgrade-built-in t
              package-archives '(("melpa" . "https://melpa.org/packages/")
                                 ("gnu" . "https://elpa.gnu.org/packages/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Declare interactive functions used at startup to inform the byte-compiler
(let ((startup-buffer 'denote-journal-new-or-existing-entry))
  (declare-function denote-journal-new-or-existing-entry "denote-journal"
                    (&optional date))
  (setq-default initial-buffer-choice startup-buffer))

;;; Store customizations separately
(let ((fp (locate-user-emacs-file "custom.el")))
  (unless (file-exists-p fp)
    (make-empty-file fp))
  (setq-default custom-file fp)
  (load custom-file nil 'nomessage))

;;; Load modules
(require 'init-system)
(require 'init-gui-frames)
(require 'init-org)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
(require 'init-gpt)

(provide 'init)
