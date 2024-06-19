;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;;
;;
;; Modus Themes
;;
;; Load the built-in theme before customization
(require-theme 'modus-themes)

(setq modus-themes-common-palette-overrides
      '(
	;; Make the mode line borderless
	(border-mode-line-active bg-mode-line-active)
	(border-mode-line-inactive bg-mode-line-inactive)

	;; Set color faces for `display-line-numbers-mode'
	(fg-line-number-inactive "gray50")
	(fg-line-number-active fg-main)
	(bg-line-number-inactive unspecified)
	(bg-line-number-active bg-hl-line)

	;; Make the fringe invisible
	(fringe unspecified)

	;; Subtle underlines
	(underline-link border)
	(underline-link-visited border)
	(underline-link-symbolic border)

	;; Make links the same color as `fg-main'
	;; This also affects `button' faces in Modus Themes
	(fg-link unspecified)
	(fg-link-visited unspecified)

	;; Completions
	;; (fg-completion-match-0 fg-main)
	;; (bg-completion-match-0 bg-red-intense)

	(bg-paren-match bg-green-intense)
	;; (underline-paren-match fg-main)

	;; Make DONE less intense
	(prose-done fg-dim)

	;; Custom region colors
	;; (bg-region bg-red-intense)
	(fg-region unspecified)))

(setq modus-themes-prompts '(extrabold)
      modus-themes-completions '((t . (extrabold))))

;; diable other themes before loading Modus Themes
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confirm)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.15)

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;; Cursor faces
(setq-default cursor-type '(bar . 1))
(setq blink-cursor-mode nil)

;; highlight current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;; Custom font
(set-face-attribute 'default nil :family "Sthenno Mono" :height 140)

;; Font ligatures support
(use-package ligature :straight t
  :config
  (ligature-set-ligatures
   't
   '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-"
     "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<===" "<<=" "<="
     "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->"
     "<=>" "<==>" "<===>" "<====>"
     "::" ":::" "__" "..." ".."
     "<~~" "</" "</>" "/>" "~~>"
     "==" "!=" "<>" "===" "!==" "!==="
     "<:" ":=" "*=" "*+" "<*" "<*>" "*>"
     "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
     "(* *)" "/*" "*/" "[|" "|]" "{|" "|}"
     "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---"))
  (global-ligature-mode 1))

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;; Mode Line settings
(setq mode-line-compact t)
(setq line-number-mode nil)

;; Make Dired more colorful
;; (use-package diredfl
;;   :straight t
;;   :config (diredfl-global-mode 1))

;; (add-hook 'dired-mode-hook #'(lambda ()
;; 			       (dired-hide-details-mode 1)
;; 			       (dired-sort-toggle-or-edit)))


;; Better margins
(use-package perfect-margin
  :straight t
  :diminish (perfect-margin-mode)
  :config
  (perfect-margin-mode 1)
  (setq perfect-margin-ignore-filters nil)
  (setq perfect-margin-ignore-regexps nil)

  ;; Add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

(provide 'init-gui-frames)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
