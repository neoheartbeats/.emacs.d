;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; number of other files.
;; This file is inspired by `https://github.com/purcell/emacs.d/'.

;;; Code:


;; Speed up startup
(setq auto-mode-case-fold nil)


;; Adjust garbage collection thresholds during startup
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 64 1024 1024))

;; --debug-init implies `debug-on-error'
(setq debug-on-error init-file-debug)


;; Bootstrap process
;; Setup `custom.el'
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load path

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path "/Users/ilyaw39/Developer/dev-emacs/dev-org/lisp/")

;; Load essential components
(require 'cl-lib)
(require 'init-packages)

;; Load components
(require 'init-macos)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-minibuff)
(require 'init-corfu)
(require 'init-temp)
(require 'init-utils)
(require 'init-editing-utils)
(require 'init-projects)
(require 'init-org)
;; (require 'init-tex)
(require 'init-eglot)


(provide 'init)
;;; init.el ends here
