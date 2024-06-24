;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; The content of this file mainly includes an Emacs UI design optimized
;; for GUI mode. This file mainly configures `modus-themes', fonts, some
;; global `face' and `mode-line' styles. For those feature-specific
;; `faces', their configuration code is isolated. A typical example is
;; `prettify-symbols-mode'. See also: `init-editing-utils', `init-org'.
;;
;;; Code:

;;;
;;
;; Modus Themes
(use-package modus-themes
  :straight t
  :config
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

	      (bg-paren-match bg-green-intense)

	      ;; Make DONE less intense
	      (prose-done fg-dim)

	      ;; Custom region colors
	      (fg-region unspecified)))

  (setq modus-themes-prompts '(extrabold)
	    modus-themes-completions '((t . (extrabold))))

  ;; Enable and load the theme
  (modus-themes-load-theme 'modus-vivendi))


;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;; Cursor faces
(setq-default cursor-type '(bar . 1))
(setq blink-cursor-mode nil)

;; highlight current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;; Custom font
(set-face-attribute 'default nil :family "Sthenno Mono" :height 150)

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)

;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))


;;; Mode Line settings
(setq mode-line-compact t)
(setq line-number-mode nil)

;; MLScroll
(use-package mlscroll
  :straight t
  :config
  (setq mlscroll-out-color "#989898") ; `fg-dim' of `modus-vivendi'
  (mlscroll-mode 1))

;; Removal of mule-info, and a trim of all double-spaces anywhere in the mode line
;; format to a single space
(setq-default
 mode-line-format
 (cl-nsubst-if " " (lambda (x) (and (stringp x) (string-blank-p x) (> (length x) 1)))
		       (remove 'mode-line-mule-info mode-line-format)))

(use-package cyphejor
  :straight t
  :config
  (setq cyphejor-rules
        '(:upcase
          ("buffer"      "β")
          ("diff"        "Δ")
          ("dired"       "􀩚")
          ("emacs"       "E")
          ("lisp"        "L" :postfix)
          ("menu"        "􀓕" :postfix)
          ("mode"        "")
          ("shell"       "􀩼")
          ("text"        "T")
          ("org"         "􀐘")))
  (cyphejor-mode 1))


;; Automatic adjusting for margins
;;
;; (use-package perfect-margin
;;   :straight t
;;   :diminish (perfect-margin-mode)
;;   :config (perfect-margin-mode 1))

(use-package spacious-padding
  :straight t
  :config (spacious-padding-mode 1))


;; Sthenno's version of https://github.com/doomemacs/doomemacs/tree/master/modules/ui/doom-quit
(defvar sthenno-quit-messages
  `(
    "Anyone else but you?"
    "She depends on you."
    "Please take care of Sthenno."
    "It's not like I'll miss you or anything, b-baka!"
    "Please don't go!")
  "A list of quit messages, picked randomly by `sthenno-quit'.")

(defun sthenno-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))

(defun sthenno-quit-fn (&rest _)
  (sthenno-quit-p
   (format "%s  %s"
           (propertize (nth (random (length sthenno-quit-messages))
                            sthenno-quit-messages)
                       'face '(default))
           "Really quit Emacs?")))

(setq confirm-kill-emacs #'sthenno-quit-fn)

(provide 'init-gui-frames)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
