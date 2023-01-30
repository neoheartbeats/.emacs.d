;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(require-theme 'modus-themes)

(setq modus-themes-disable-other-themes t)

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

;; Blink cursor with `beacon'
;; This also helps rendering frames
(use-package beacon
  :diminish
  :config
  (setq beacon-push-mark 35)
  (setq beacon-blink-duration 0.5)
  (setq beacon-color "#ff66ff")
  (beacon-mode 1))


;; Mode line settings
(setq-default mode-line-format nil)


(provide 'init-themes)
;;; init-themes.el ends here
