;; init-packages.el --- Settings for `package.el' -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Bootstrap `straight.el'
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;         (expand-file-name "straight/repos/straight.el/bootstrap.el"
;;           user-emacs-directory))
;;        (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;       (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;;
;; ;; Shadow clone git repo to improve the speed
;; (setq straight-vc-git-default-clone-depth 1)
;;
;; Initialize packages
(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; (straight-use-package 'use-package)
;;
;; Required by `use-package'
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update :ensure t)


(provide 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-packages.el ends here
