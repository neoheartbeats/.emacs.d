;; ui.el --- Lyrith: loading first -*- lexical-binding: t -*-
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
;; Remove uneccessary components
;;
;; Remove frame components
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set default frame size & position
(if (display-graphic-p)
    (setq initial-frame-alist
          '((top . 0)
	        (width . 100)
	        (height . 50))))

(setq default-frame-alist
      '((top . 0)
	    (width . 100)
	    (height . 50)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default message startup
(defun display-startup-echo-area-message ()
  (message "Funding for this program is made possible by viewers like you."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cursor settings
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))

;; Highlight lines
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font settings
(set-face-attribute 'default nil
		            :font "FIRA CODE"
		            :height 150)
(set-face-attribute 'variable-pitch nil
		            :font "Noto Serif CJK SC"
		            :height 150)
(set-fontset-font t 'han "Noto Serif CJK SC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus themes
(use-package modus-themes
  :diminish t
  :init
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mode-line '(borderless))
  (modus-themes-syntax '(yellow-comments green-strings))
  (modus-themes-completions 'opinionated)
  (modus-themes-hl-line '(underline))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-links '(neutral-underline))
  (modus-themes-box-buttons '(variable-pitch 0.9))
  (modus-themes-prompts '(intense)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode line
;;
;; Diminish uneccessary modes
(use-package diminish)
(defun diminish-modes ()
  (require 'diminish)
  (diminish 'company-mode)
  (diminish 'global-company-mode)
  (diminish 'smartparens-mode)
  (diminish 'yas-minor-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'org-fragtog-mode)
  (diminish 'org-bullets-mode)
  (diminish 'org-fancy-priorities-mode)
  (diminish 'visual-line-mode)
  (diminish 'org-indent-mode)
  (diminish 'auto-revert-mode))
(add-hook 'after-init-hook 'diminish-modes)

;; Smart mode line
(use-package smart-mode-line
  :init
  (sml/setup))

(provide 'ui)

;; ui.el ends here
