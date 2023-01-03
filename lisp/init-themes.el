;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package modus-themes)
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confim)


;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground nil)

(set-face-background 'line-number nil)
(set-face-background 'line-number-current-line nil)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 15)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))


;; Mode line settings
(setq-default mode-line-compact t)
;; (setq-default mode-line-format nil)

(diminish 'eldoc-mode)


(provide 'init-themes)
;;; init-themes.el ends here
