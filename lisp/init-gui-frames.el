;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Optimization
(setq idle-update-delay 1.0)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq frame-resize-pixelwise t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus Themes
(require-theme 'modus-themes)
(setq modus-themes-custom-auto-reload nil)
(setq modus-themes-italic-constructs nil)
(setq modus-themes-bold-constructs nil)
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified) ; Remove the border of mode-line
        (border-mode-line-inactive unspecified)
        (fringe unspecified) ; Make the fringe invisible
        (underline-link border) ; Subtle underlines
        (underline-link-visited border)
        (underline-link-symbolic border)
        (string green-cooler) ; Use green strings
        (bg-hover bg-green-subtle) ; Make the background subtle green
        (bg-line-number-inactive unspecified) ; Make line numbers less intense
        (bg-line-number-active unspecified)))
(load-theme 'modus-vivendi :no-confirm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground 'unspecified)

(set-face-attribute 'link nil :foreground 'unspecified)
(set-face-attribute 'fill-column-indicator nil :height 0.15)
(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))
(set-cursor-color "#ff66ff")

;; highlight current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom font
;;
;; Main typeface
(set-face-attribute 'default nil :family "Pes Mono" :height 150)

;; Font settings
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")

(set-face-attribute 'italic nil :slant 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default startup message
(defun display-startup-echo-area-message ()
  (let ((text "Funding for this program was made possible by viewers like you."))
    (message "􀪾 %s" text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Line settings
(setq-default line-number-mode nil)


(provide 'init-gui-frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-gui-frames.el ends here
