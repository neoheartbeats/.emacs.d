;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration. It handles
;; core settings, performance optimizations, and loads module-specific configurations
;; from `user-lisp/'.

;;; Code:

;;; Process and display performance
(setq-default save-silently t
              remote-file-name-inhibit-locks t
              backup-inhibited t
              redisplay-skip-fontification-on-input t
              frame-inhibit-implied-resize t
              load-prefer-newer t
              fill-column 88
              mode-line-format ""
              header-line-format "")

(setq-default user-full-name user-login-name
              user-mail-address "sthenno@sthenno.com")

;;; Basic startup settings
(setq-default inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-buffer-menu t
              inhibit-default-init t
              initial-scratch-message "Hi!")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((text (propertize
               "Funding for this program was made possible by viewers like you."
               'face 'default)))
    (minibuffer-message " %s" text)))

;;; Package management
(setopt package-native-compile t
        package-install-upgrade-built-in t
        package-archives '(("gnu-devel" . "https://elpa.gnu.org/devel/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))

;;; Declare interactive functions used at startup to inform the byte-compiler
(let ((startup-buffer #'denote-journal-new-or-existing-entry))
  (declare-function denote-journal-new-or-existing-entry "denote-journal"
                    (&optional date))
  (setopt initial-buffer-choice startup-buffer))

;;; Load modules
(require 'init-system)
(require 'init-gui-frames)
(require 'init-org)
(require 'init-projects)
(require 'init-temp)
(require 'init-comp)
(require 'init-eglot)
(require 'init-gpt)

;;; Store customizations separately
(let ((fp (locate-user-emacs-file "custom.el")))
  (unless (file-exists-p fp)
    (make-temp-file fp))
  (setq-default custom-file fp))

(provide 'init)
