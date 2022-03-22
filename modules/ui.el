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
          '((top . 75)
            (left . 165)
	        (width . 130)
	        (height . 42)
            (alpha . (85 . 60)))))
(setq default-frame-alist
      '((top . 75)
        (left . 165)
	    (width . 130)
	    (height . 42)
        (alpha . (85 . 60))))

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
(set-face-attribute 'default nil
                    :font "Fira Code"
                    :height 155)

(set-face-attribute 'variable-pitch nil
                    :font "Fira Sans"
                    :height 170)

(set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")

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
  (modus-themes-hl-line '(underline accented))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-links '(neutral-underline))
  (modus-themes-box-buttons '(variable-pitch 0.9))
  (modus-themes-prompts '(intense)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hide mode line
(setq-default mode-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Animations
;;
;; Special animations
(mac-start-animation nil
                     :type 'page-curl-with-shadow
                     :duration 0.75
                     :direction 'right
                     :angle 45)
(mac-start-animation (selected-window)
                     :type 'move-out
                     :duration 0.75
                     :direction 'right)

;; Implement fade-outs
(defcustom mac-animation-duration 0.3
  "Duration of transition animations")
(defvar mac-animation-locked-p nil)
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))

(defun animate-frame-fade-out (&rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration mac-animation-duration)
    (run-with-timer mac-animation-duration nil 'mac-animation-toggle-lock)))

(advice-add 'set-window-buffer :before 'animate-frame-fade-out)
(advice-add 'split-window :before 'animate-frame-fade-out)
(advice-add 'delete-window :before 'animate-frame-fade-out)
(advice-add 'delete-other-windows :before 'animate-frame-fade-out)
(advice-add 'window-toggle-side-windows :before 'animate-frame-fade-out)

(provide 'ui)

;; ui.el ends here
