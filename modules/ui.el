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
;; UI setup.
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
          '((top . 65)
	        (width . 95)
	        (height . 42))))
(setq default-frame-alist
      '((top . 65)
	    (width . 95)
	    (height . 42)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Identity
;;
;; Default startup message
(defun display-startup-echo-area-message ()
  (message "Funding for this program is made possible by viewers like you."))

;; Set title text
(setq frame-title-format "GccEmacs@28.0.91. Lyrith is built for you.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cursor settings
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))

;; Highlight lines
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rainbow delimiters
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font settings
(set-face-attribute 'default nil
		            :font "FIRA CODE"
		            :height 155)
(set-fontset-font t 'han "Heiti SC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus themes
(use-package modus-themes
  :delight
  :init
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-mode-line '(borderless))
  (modus-themes-syntax '(green-strings))
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
;; Diminish native minor modes
(use-package diminish
  :config
  (defun diminish-minor-mode ()
    (diminish 'visual-line-mode)
    (diminish 'org-indent-mode)
    (diminish 'auto-revert-mode))
  :hook
  (after-init . diminish-minor-mode))

(provide 'ui)

;; ui.el ends here
