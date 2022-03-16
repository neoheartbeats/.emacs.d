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
          '((top . 70)
            (left . 180)
	        (width . 120)
	        (height . 42)
            (alpha . (90 . 60)))))
(setq default-frame-alist
      '((top . 70)
        (left . 180)
	    (width . 120)
	    (height . 42)
        (alpha . (90 . 60))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default startup message
(defun display-startup-echo-area-message ()
  (message "Funding for this program was made possible by viewers like you."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean frame title
;;
;; Set frame text
(setq frame-title-format nil)

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
(set-frame-font "FIRA CODE-15.5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus themes
(use-package modus-themes
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
;; Hide mode line
(setq-default mode-line-format nil)

(provide 'ui)

;; ui.el ends here
