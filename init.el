;; init.el --- Credits: Lyrith -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file provides the basis & structure that draws Emacs configuration.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package pre-loaded process
;;
;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
	     (expand-file-name "straight/repos/straight.el/bootstrap.el"
					               user-emacs-directory))
			(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
			  (url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the frame parameters before it's drawing
(setq default-frame-alist
      '((top . 145)
        (left . 275)
	      (width . 145)
	      (height . 40)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load modules
;;
;; Load path
(add-to-list 'load-path "~/.emacs.d/modules/")

;; Load components
(require 'init-utils)
(require 'init-defaults)
(require 'init-enhance)
(require 'init-gui)
(require 'init-org)
(require 'init-tex)
(require 'init-ox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup custom file
(setq custom-file "~/.emacs.d/modules/custom.el")
(load custom-file 'no-error 'no-message)
