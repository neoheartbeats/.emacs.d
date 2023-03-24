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

;; Fix the error while installing `straight.el'
(defvar native-comp-deferred-compilation-deny-list nil)

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
;; Develop version for Org Mode
(use-package org
  :straight `(org
               :fork (
                       :host nil
                       :repo "https://git.tecosaur.net/tec/org-mode.git"
                       :branch "dev"
                       :remote "tecosaur")
               :files (
                        :defaults "etc")
               :build t
               :pre-build
               (with-temp-file "org-version.el"
                 (require 'lisp-mnt)
                 (let (
                        (version
                          (with-temp-buffer
                            (insert-file-contents "lisp/org.el")
                            (lm-header "version")))
                        (git-version
                          (string-trim
                            (with-temp-buffer
                              (call-process "git" nil t nil
                                "rev-parse"
                                "--short"
                                "HEAD")
                              (buffer-string)))))
                   (insert
                     (format
                       "(defun org-release () \"The release version of Org.\" %S)\n"
                       version)
                     (format
                       "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n"
                       git-version)
                     "(provide 'org-version)\n")))
               :pin nil))

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
