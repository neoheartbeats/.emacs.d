;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confim)


;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground nil)

(set-face-attribute 'fill-column-indicator nil
                    :height 0.15)

(set-face-background 'line-number nil)
(set-face-background 'line-number-current-line nil)
(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))
(set-cursor-color "#ff5f5f")


;; Mode line settings
(setq-default mode-line-compact t)



(provide 'init-themes)
;;; init-themes.el ends here
