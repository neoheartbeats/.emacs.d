;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(load-theme 'modus-vivendi t)


;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground nil)

(set-face-attribute 'fill-column-indicator nil
                    :height 0.15)

(set-face-attribute 'link nil
                    :foreground nil)

(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))

(set-cursor-color "#ff66ff")


;; Mode line settings
(setq mode-line-compact t)


;; Font settings
(set-face-attribute 'default nil
	            :font "M PLUS 1 Code"
	            :height 150)

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "HarmonyOS Sans SC")

(set-face-attribute 'italic nil :slant 'normal)


(provide 'init-themes)
;;; init-themes.el ends here
