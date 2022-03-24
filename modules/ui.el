;; ui.el --- Credits: loading first -*- lexical-binding: t -*-
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
;; Default startup message
(defun display-startup-echo-area-message ()
  (message "Funding for this program was made possible by viewers like you."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame title setup
;;
;; Show icon & full path in title bar
(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (abbreviate-file-name buffer-file-name)
          "%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cursor settings
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
                    :font "M PLUS 1 Code"
                    :height 165)

(set-face-attribute 'variable-pitch nil
                    :font "Times"
                    :height 180)

(set-fontset-font "fontset-default" 'han "Songti SC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus themes
(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-subtle-line-numbers t)
  (modus-themes-lang-checkers '(straight-underline))
  (modus-themes-mode-line '(borderless))
  (modus-themes-syntax '(green-strings))
  (modus-themes-completions 'moderate)
  (modus-themes-hl-line '(underline accented))
  (modus-themes-paren-match '(underline intense))
  (modus-themes-links '(neutral-underline no-color))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-box-buttons '(variable-pitch 0.9))
  (modus-themes-prompts '(intense background))
  ;; (modus-themes-completions '((matches . (bold))
  ;;                             (selection . (semibold accented))
  ;;                             (popup . (accented intense))))
  :config
  (modus-themes-load-vivendi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use single line as modeline
(use-package emacs
  :custom-face
  (mode-line ((t (:height 0.1))))
  (mode-line-inactive ((t (:inherit mode-line))))
  :config
  (setq-default mode-line-format '("")))

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
(defcustom mac-animation-duration 0.25
  "Duration of transition animations")
(defvar mac-animation-locked-p nil)
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))

(defun animate-frame-fade-out (&rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil
                         :type 'fade-out
                         :duration mac-animation-duration)
    (run-with-timer mac-animation-duration nil
                    'mac-animation-toggle-lock)))

(advice-add 'set-window-buffer
            :before 'animate-frame-fade-out)
(advice-add 'split-window
            :before 'animate-frame-fade-out)
(advice-add 'delete-window
            :before 'animate-frame-fade-out)
(advice-add 'delete-other-windows
            :before 'animate-frame-fade-out)
(advice-add 'window-toggle-side-windows
            :before 'animate-frame-fade-out)

(provide 'ui)
