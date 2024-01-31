;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Modus Themes
;;
(require-theme 'modus-themes)

(setq modus-themes-custom-auto-reload t)
(setq modus-themes-disable-other-themes t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified) ; No borders for mode lines
        (underline-link border) ; Subtle underlines
        (underline-link-visited border)
        (underline-link-symbolic border)
        (fg-line-number-inactive "gray50") ; Subtle line numbers
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)))

;; Diable other themes before loading Modus Themes
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confirm)

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
(set-face-attribute 'default nil :family "Signoir" :height 140)

;; Font ligatures support
(use-package ligature
  :straight t
  :config
  (ligature-set-ligatures 't
                          '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-"
                            "->" "->>" "-->" "--->" "->-" ">-" ">>-"
                            "=<<" "=<" "=<=" "<==" "<===" "<<=" "<="
                            "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
                            "<->" "<-->" "<--->" "<---->"
                            "<=>" "<==>" "<===>" "<====>"
                            "::" ":::" "__"
                            "<~~" "</" "</>" "/>" "~~>"
                            "==" "!=" "<>" "===" "!==" "!==="
                            "<:" ":=" "*=" "*+" "<*" "<*>" "*>"
                            "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
                            "(* *)" "/*" "*/" "[|" "|]" "{|" "|}"
                            "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---"))
  (global-ligature-mode 1))

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'unicode "SF Pro Display")
(set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")

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
