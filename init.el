;;; init.el --- Load the full configuration  -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; number of other files.
;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time


;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 4 1024 1024))

;; --debug-init implies `debug-on-error'
(setq debug-on-error init-file-debug)


;; Bootstrap config
;; Setup `custom.el'
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load basic components
(require 'init-utils)
(require 'init-straight)

;; Load configs for specific features and modes
(straight-use-package 'diminish)


;; Load components
(require 'init-osx)
(require 'init-fonts)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-minibuff)
(require 'init-corfu)
(require 'init-editing-utils)
(require 'init-org)
(require 'init-tex)


(provide 'init)
;;; init.el ends here
