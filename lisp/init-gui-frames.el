;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 KAMUSUSANOWO

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Modus Themes
;;
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified) ; No borders for mode lines
          (underline-link border) ; Subtle underlines
          (underline-link-visited border)
          (underline-link-symbolic border)
          (fg-line-number-inactive "gray50") ; Subtle line numbers
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)))
  :config (load-theme 'modus-vivendi :no-confirm))

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground 'unspecified)

(set-face-attribute 'link nil :foreground 'unspecified)
(set-face-attribute 'fill-column-indicator nil :height 0.15)
(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default cursor-type '(bar . 1))
(setq-default blink-cursor-mode nil)

;; highlight current line
(add-hook 'after-init-hook #'(lambda ()
                               (global-hl-line-mode 1)))

;;
;; Custom font
;;
(set-face-attribute 'default nil :family "Romantica" :height 140)

;; Ligatures support
(use-package ligature
  :straight t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
                                       "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>"
                                       ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "==="
                                       "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>"
                                       "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
   (ligature-set-ligatures 'text-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
                                       "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>"
                                       ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "==="
                                       "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>"
                                       "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode 1))

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;;
;; Mode Line settings
;;
(setq-default mode-line-compact t)
(setq-default line-number-mode nil)

(provide 'init-gui-frames)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
