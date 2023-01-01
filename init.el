;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; number of other files.
;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Initial global constants
(defvar *macos-q* (string= system-type "darwin"))
(defvar *const-q* (> emacs-major-version 28))


;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 64 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 32 1024 1024))

;; --debug-init implies `debug-on-error'
(setq debug-on-error init-file-debug)


;;; Bootstrap process
;; Setup `custom.el'
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load essential components
(require 'init-packages)

;; Load components
(require 'init-macos)
(require 'init-fonts)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-minibuff)
(require 'init-corfu)
(require 'init-temp)
(require 'init-utils)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-org)
(require 'init-tex)
(require 'init-eglot)


(provide 'init)
;;; init.el ends here
