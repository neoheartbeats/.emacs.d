;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; 这个文件的内容主要包含了针对 GUI 模式下优化的 Emacs UI 设计.
;; 本文件主要配置了 `modus-themes', 字体, 一些全局的 `face' 和 `mode-line' 的样式.
;; 对于那些功能性专有的 `face', 它们的配置代码是被隔离开的, 典型的例子是 `prettify-symbols-mode'.
;; See also `init-editing-utils', `init-org'.
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

	  ;; Yellow comments and green strings
	  (comment yellow-cooler)
	  (string green-cooler)

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
(set-face-attribute 'default nil :family "Monaco" :height 140)

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))


;; Mode Line settings
(setq mode-line-compact t)
(setq line-number-mode nil)


;; Automatic adjusting for margins
(use-package perfect-margin
  :straight t
  :diminish (perfect-margin-mode)
  :config
  (setq perfect-margin-ignore-filters nil
	perfect-margin-ignore-regexps nil)
  (perfect-margin-mode 1))

(provide 'init-gui-frames)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
