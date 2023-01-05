;;; init-packages.el --- Settings for package.el -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps `package.el' and `use-package.el'.
;; This file should be placed before installing any packages.

;;; Code:

(require 'cl-lib)
(require 'package)


;; Setup standard package repositories
(setq-default package-archives
              '(("melpa" . "https://melpa.org/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")))


;; Bootstrap `use-package.el'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package bind-key :demand t)

(use-package diminish
  :demand t
  :config
  (diminish 'eldoc-mode))


;; Keep `user-emacs-directory' clean
(use-package no-littering :demand t)


(provide 'init-packages)
;;; init-packages ends here
