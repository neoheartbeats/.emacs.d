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
(eval-when-compile ; Required by `use-package'
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-minimum-reported-time 0.01
        use-package-enable-imenu-support t)
  (require 'use-package))

;; Packages bundled with `use-package'
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update :ensure t)


(provide 'init-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-packages.el ends here
