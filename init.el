;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; number of other files.
;; This file is inspired by `https://github.com/purcell/emacs.d/'.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Speed up startup
;;
;; Defer garbage collection further back in the startup process
;; This would be changed by `gcmh' from `init-system.el'
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flash of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
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
  (dolist (subdirs '("site-lisp/" "lisp/"))
    (push (expand-file-name subdirs user-emacs-directory) load-path)))

(defun pes-push-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
         (expand-file-name "site-lisp/" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'pes-update-load-path)
(advice-add #'package-initialize :after #'pes-push-subdirs-to-load-path)

(pes-update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load essential components
;; (require 'init-custom)
(require 'init-packages)

(global-set-key
 (kbd "<f12>")
 #'(lambda ()
     (interactive)
     (find-file (expand-file-name "init.el" user-emacs-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load components
(require 'init-system)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-editing-utils)
(require 'init-minibuff)
(require 'init-corfu)
(require 'init-temp)
;; (require 'init-utils)
;; (require 'init-projects)
;; (require 'init-org)
;; (require 'init-eglot)


;;; init.el ends here
