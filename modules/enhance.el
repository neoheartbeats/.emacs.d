;; enhance.el --- Lyrith: loading first -*- lexical-binding: t -*-
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
;;

(use-package company
  :init
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
	("<escape>" . company-abort)
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  (:map company-search-map
	("<escape>" . company-search-abort)
	("C-p" . company-select-previous)
	("C-n" . company-select-next)))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("M-s i" . consult-imenu)
   ("M-s g" . consult-grep)))

(use-package magit)

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  (yas-triggers-in-field t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'enhance)
