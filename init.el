;; init.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Essentials must be loaded first.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package pre-loaded process
;;
;; Determine package loading sources
(require 'package)
(setq package-user-dir "~/.emacs.d/packages")
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
	    ("elpa" . "https://elpa.gnu.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Default to ensure installing packages
(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load modules
;;
;; Load path
(add-to-list 'load-path "~/.emacs.d/modules/")

;; Load components
(require 'core)
(require 'defaults)
(require 'enhance)
(require 'ui)
(require 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set custom file
(setq custom-file "~/.emacs.d/modules/custom.el")
(load custom-file 'no-error 'no-message)

;; init.el ends here
