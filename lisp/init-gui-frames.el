;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;;
;;
;; Modus Themes
;;
;; Load the built-in theme before customization
(require-theme 'modus-themes)

(setopt modus-themes-bold-constructs t)
(setopt modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
(setopt modus-themes-common-palette-overrides
        `((cursor red)
          
          ;; Set color faces for `display-line-numbers-mode'
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          ;; Make the fringe invisible
          (fringe unspecified)

          ;; Subtle underlines
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)

          ;; Completions
          (fg-completion-match-0 fg-main)
          (bg-completion-match-0 bg-red-intense)

          (bg-paren-match bg-red-intense)
          (underline-paren-match fg-main)

          ;; Make DONE less intense
          (prose-done fg-dim)

          ;; Custom region colors 
          (bg-region bg-red-intense)
          (fg-region unspecified)

          ;; Add the code to `modus-themes-preset-overrides-intense'
          ,@modus-themes-preset-overrides-intense))

(setopt modus-themes-prompts '(extrabold))
(setopt modus-themes-completions '((t . (extrabold))))

;; diable other themes before loading Modus Themes
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confirm)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.15)

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;; Cursor faces
(setopt cursor-type '(bar . 1))
(setopt blink-cursor-mode nil)

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

;; Turn on `prettify-symbols-mode' in all supported buffers
(add-hook 'after-init-hook #'(lambda ()
                               (global-prettify-symbols-mode 1)))

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;;
;; Mode Line settings
;;
(setopt mode-line-compact t)
(setopt line-number-mode nil)

(provide 'init-gui-frames)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
