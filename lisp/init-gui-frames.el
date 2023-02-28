;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file is inspired by https://github.com/purcell/emacs.d/.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq frame-resize-pixelwise t)
(setq redisplay-skip-fontification-on-input t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Identity
(use-package
 modus-themes
 :init (setq modus-themes-disable-other-themes t)
 :config (load-theme 'modus-vivendi t))

(setq frame-title-format '("Pes 􀄫 %b"))
(setq icon-title-format frame-title-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize faces
(set-face-attribute 'button nil :underline "#959595" :foreground 'unspecified)
(set-face-attribute 'link nil :foreground 'unspecified)

(set-face-attribute 'fill-column-indicator nil :height 0.15)

(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))

(set-cursor-color "#ff5f59")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode line settings
;;
;; The basic mode line pack
(use-package
 minions
 :custom (minions-mode-line-lighter "􀌞")
 :config (minions-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom font
;;
;; Main typeface
(set-face-attribute 'default nil :family "Pes Mono" :height 155)

;; Font settings
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

(set-face-attribute 'italic nil :slant 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default startup message
(defun display-startup-echo-area-message ()
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Making frames
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'."
  (with-selected-frame frame
    (run-hooks
     (if window-system
         'after-make-window-system-frame-hooks
       'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions #'run-after-make-frame-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The initial frame
(defconst pes-initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")
(add-hook
 'after-init-hook
 #'(lambda ()
     (when pes-initial-frame
       (run-after-make-frame-hooks pes-initial-frame))))
(setq switch-to-buffer-obey-display-actions t)

(provide 'init-gui-frames)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-gui-frames.el ends here
