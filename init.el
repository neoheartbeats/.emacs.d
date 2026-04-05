;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the main initialization file that bootstraps the configuration.
;; It handles core settings, performance optimizations, and loads
;; module-specific configurations from `user-lisp/'.

;;; Code:
;;

(defconst sthenno/startup-modules
  '(init-system
    init-gui-frames
    init-tex
    init-org
    init-editing-utils
    init-projects
    init-temp
    init-comp
    init-eglot
    init-gpt)
  "Features loaded by `init.el'.")

;; Process and display performance.
(setopt read-process-output-max (* 4 1024 1024)
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

;; Basic startup settings.
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message nil)

(setopt user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com"
        elisp-fontify-semantically t)

(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (let ((icon (propertize "􀎛" 'face 'default))
        (text "它们没能得到答案，只能看到凋零的生命。"))
    (message "%s %s" icon text)))

;; Package management.
(setopt package-vc-allow-build-commands t
        package-install-upgrade-built-in t
        package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        use-package-enable-imenu-support t
        use-package-compute-statistics t
        use-package-vc-prefer-newest t)

(require 'use-package)

;; Declare interactive functions used at startup to inform the byte-compiler
(let ((startup-buffer 'denote-journal-new-or-existing-entry))
  (declare-function denote-journal-new-or-existing-entry "denote-journal"
                    (&optional date))
  (setopt initial-buffer-choice startup-buffer))

;; Store customizations separately
(let ((fp (locate-user-emacs-file "custom.el")))
  (unless (file-exists-p fp)
    (make-empty-file fp))
  (setopt custom-file fp)
  (load custom-file nil 'nomessage))

(dolist (feature sthenno/startup-modules)
  (require feature))

(require 'server)
(unless (server-running-p)
  (server-start))
