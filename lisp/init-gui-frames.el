;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file is inspired by https://github.com/purcell/emacs.d/.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq frame-resize-pixelwise t)
(setq redisplay-skip-fontification-on-input t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Identity
(use-package modus-themes :ensure t
  :init (setq modus-themes-disable-other-themes t)
  :config (load-theme 'modus-vivendi t))

(setq frame-title-format '("Pes 􀄫 %b"))
(setq icon-title-format frame-title-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize faces
(set-face-attribute 'button nil :underline "#959595" :foreground 'unspecified)
(set-face-attribute 'link nil :foreground 'unspecified)

(set-face-attribute 'fill-column-indicator nil :height 0.15)

(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))

(set-cursor-color "#ff5f59")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode line settings
;;
;; The basic mode line pack
(use-package minions :ensure t
  :custom (minions-mode-line-lighter "􀌞")
  :config (minions-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom font
;;
;; Main typeface
(set-face-attribute 'default nil :family "Source Code Pro" :height 145)

;; Font settings
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

(set-face-attribute 'italic nil :slant 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default startup message
(defun display-startup-echo-area-message () (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

(provide 'init-gui-frames)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-gui-frames.el ends here
