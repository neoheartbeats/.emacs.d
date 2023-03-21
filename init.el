;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file bootstraps the configuration.
;;
;; This file is inspired by `https://github.com/purcell/emacs.d/'.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Speed up startup
;;
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(setq max-lisp-eval-depth 10000)
(setq auto-mode-case-fold nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap process
;;
;; Load path
;;
;; Force `lisp' and `site-lisp' at head to reduce the startup time
(defun pes-update-load-path (&rest _)
  "Update `load-path'."
  (dolist (subdirs '("lisp/"))
    (push (expand-file-name subdirs user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'pes-update-load-path)
(pes-update-load-path)

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Site packages
(use-package org :load-path "site-lisp/org-mode/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load components
(require 'init-system)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-minibuff)
(require 'init-corfu)
(require 'init-temp)
(require 'init-projects)
(require 'init-org)
(require 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init.el ends here
