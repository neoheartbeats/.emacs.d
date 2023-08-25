;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-
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
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1.0)
(setq frame-resize-pixelwise t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modus Themes
(use-package modus-themes
  :ensure t ; The latest version is preferred
  :init
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified) ; No borders for mode lines
          (underline-link border) ; Subtle underlines
          (underline-link-visited border)
          (underline-link-symbolic border)
          (string green-cooler)))
  :config
  (load-theme 'modus-vivendi-tritanopia :no-confirm))

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
(set-cursor-color "#ff66ff")
(setq-default cursor-type '(bar . 1))
(setq-default blink-cursor-mode nil)

;; highlight current line
(add-hook 'after-init-hook #'(lambda ()
                               (global-hl-line-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom font
;;
;; Main typeface
(set-face-attribute 'default nil :family "GussieMono Nerd Font" :height 160)

;; Font settings
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")


;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Add font ligatures support
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures '(prog-mode text-mode)
                          '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->"
                            "<-->" "<--->" "<---->" "<!--" "<==" "<===" "<="
                            "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>"
                            "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::"
                            "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>"
                            "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
                            "+++" "__"))
  (global-ligature-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default startup message
(defun display-startup-echo-area-message ()
  (let
      ((text "Funding for this program was made possible by viewers like you."))
    (message "􀪾 %s" text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Line settings
(setq-default mode-line-compact t)
(setq-default line-number-mode nil)

(provide 'init-gui-frames)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-gui-frames.el ends here
