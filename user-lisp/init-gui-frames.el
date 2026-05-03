;;; init-gui-frames.el --- GUI appearance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures the visual side of the Emacs UI.

;;; Code:

(use-package modus-themes
  :ensure t
  :vc (:url "https://github.com/protesilaos/modus-themes" :branch "main")
  :init (mapc #'disable-theme custom-enabled-themes)
  :config
  (setopt modus-themes-common-palette-overrides
          `((fg-line-number-active fg-dim)
            (bg-line-number-active bg-hl-line)
            (fg-line-number-inactive "#535353")
            (bg-line-number-inactive unspecified)
            (underline-link border)
            (underline-link-visited border)
            (underline-link-symbolic border)
            (fg-link unspecified)
            (fg-link-visited unspecified)
            (prose-todo info)
            (prose-done "#535353")
            ,@modus-themes-preset-overrides-faint))
  (load-theme 'modus-vivendi :no-confirm))

(set-face-attribute 'default nil :family "Tempestypes" :height 140 :weight 'light)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'italic nil :slant 'normal)
(set-face-attribute 'show-paren-match nil
                    :foreground "green"
                    :box '(:line-width (-1 . -1) :style released-button))
(set-face-attribute 'mode-line nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)
(set-face-attribute 'mode-line-active nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)
(set-face-attribute 'mode-line-inactive nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)

(let ((font "Pingfang SC"))
  (set-fontset-font t 'kana (font-spec :family font))
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag t
        cursor-type '(bar . 1)
        truncate-string-ellipsis " …")

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(blink-cursor-mode -1)

(setopt display-line-numbers-widen t
        display-line-numbers-width 4)
(global-display-line-numbers-mode 1)

(setopt show-paren-delay 0.05
        show-paren-context-when-offscreen t
        show-paren-not-in-comments-or-strings 'on-mismatch)
(show-paren-mode 1)

(setopt default-input-method nil)

(provide 'init-gui-frames)
