;;; init-gui-frames.el --- GUI appearance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures the visual side of the Emacs UI.

;;; Code:

(use-package modus-themes
  :ensure t
  :config (setopt modus-themes-common-palette-overrides
                  `(
                    ;; `display-line-numbers'
                    (fg-line-number-active fg-dim)
                    (bg-line-number-active bg-hl-line)
                    (fg-line-number-inactive "#535353")
                    (bg-line-number-inactive unspecified)

                    ;; Subtle links
                    (underline-link border)
                    (underline-link-visited border)
                    (underline-link-symbolic border)
                    (fg-link unspecified)
                    (fg-link-visited unspecified)

                    ;; Prose and completion colors
                    (prose-todo info)
                    (prose-done "#535353")
                    ,@modus-themes-preset-overrides-faint))
  (modus-themes-load-theme 'modus-vivendi))

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'italic nil :slant 'normal)

(let ((font "PingFang SC"))
  (set-fontset-font t 'kana (font-spec :family font))
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag t
        cursor-type '(bar . 1)
        truncate-string-ellipsis " …")
(setopt mode-line-compact t
        mode-line-collapse-minor-modes t
        mode-line-modes-delimiters '("[" . "]"))

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(blink-cursor-mode -1)

(setq-default display-line-numbers-widen t
              display-line-numbers-width 6)
(global-display-line-numbers-mode 1)

(setq-default show-paren-delay 0.025
              show-paren-context-when-offscreen t
              show-paren-not-in-comments-or-strings 'on-mismatch)
(show-paren-mode 1)

(setq-default default-input-method nil)

(provide 'init-gui-frames)
