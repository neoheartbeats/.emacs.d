;;; init-packages.el --- Settings for package.el -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps `package.el' and `use-package.el'.
;; This file should be placed before installing any packages.

;;; Code:

(require 'cl-lib)
(require 'package)


;; Setup standard package repositories
(setq-default package-archives
              '(
                ("melpa" . "https://melpa.org/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")))


;; Bootstrap `use-package.el'
(require 'use-package)
(setq-default use-package-always-ensure t)

(use-package diminish :demand t)
(use-package bind-key :demand t)


;; Keep `user-emacs-directory' clean
(use-package no-littering :demand t)


(provide 'init-packages)
;;; init-packages ends here
