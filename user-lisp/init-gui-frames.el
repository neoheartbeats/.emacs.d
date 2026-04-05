;;; init-gui-frames.el --- GUI appearance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures the visual side of the Emacs UI.

;;; Code:

(use-package modus-themes
  :ensure nil
  :demand t
  :config
  (setopt modus-themes-common-palette-overrides
          `(
            ;; Borderless mode line.
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)

            ;; `display-line-numbers-mode'.
            (fg-line-number-active fg-dim)
            (bg-line-number-active bg-hl-line)
            (fg-line-number-inactive "#535353")
            (bg-line-number-inactive unspecified)

            ;; Subtle links.
            (underline-link border)
            (underline-link-visited border)
            (underline-link-symbolic border)
            (fg-link unspecified)
            (fg-link-visited unspecified)

            ;; Prose and completion colors.
            (prose-todo info)
            (prose-done "#535353")
            (bg-completion bg-hl-line)

            ,@modus-themes-preset-overrides-faint))
  (modus-themes-load-theme 'modus-vivendi))

(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1 :weight 'thin)
(set-face-italic 'italic nil)

(setopt global-hl-line-sticky-flag t
        cursor-type '(bar . 1)
        truncate-string-ellipsis " …")
(setq-default mode-line-compact t
              mouse-highlight nil)

(global-hl-line-mode 1)
(blink-cursor-mode -1)

(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))

(use-package paren
  :ensure nil
  :config
  (set-face-attribute 'show-paren-match nil :inherit 'bold)
  (show-paren-mode 1))

(use-package paren-face
  :ensure t
  :config
  (set-face-attribute 'parenthesis nil :foreground "#989898")
  (global-paren-face-mode 1))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :config (global-highlight-parentheses-mode 1))

;; UTF-8 is enough for modern Emacs.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(setq default-input-method nil)

(set-face-attribute 'default nil :family "Tempestypes" :height 140)

(let ((font "PingFang SC"))
  (set-fontset-font t 'kana (font-spec :family font))
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))

(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(provide 'init-gui-frames)
