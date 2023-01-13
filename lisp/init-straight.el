;;; init-straight.el --- Settings for package.el -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps `straight.el' and `use-package.el'.
;; This file should be placed before installing any packages.

;;; Code:

(require 'cl-lib)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Shadow clone git repos to improve speed
(setq straight-vc-git-default-clone-depth 1)


(straight-use-package 'org)


;; Bootstrap `use-package.el'
(unless (package-installed-p 'use-package)
  (straight-use-package 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(setq straight-use-package-by-default t)

(use-package bind-key :demand t)

(use-package diminish
  :demand t
  :config
  (diminish 'eldoc-mode))


;; Keep `user-emacs-directory' clean
(use-package no-littering :demand t)


(provide 'init-straight)
;;; init-straight ends here
