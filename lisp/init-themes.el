;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(require 'modus-themes)
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confim)


;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground nil)

(set-face-background 'line-number nil)
(set-face-background 'line-number-current-line nil)

;; Cursor faces
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))


;; Mode line settings
(setq-default mode-line-compact t)

(diminish 'eldoc-mode)


(provide 'init-themes)
;;; init-themes.el ends here
