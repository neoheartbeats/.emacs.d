;;; init-gui-frames.el --- GUI appearance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures the visual side of the Emacs UI.

;;; Code:

(use-package modus-themes
  :ensure t
  :config (setopt modus-themes-prompts 'bold
                  modus-themes-common-palette-overrides
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

(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1 :weight 'thin)
(set-face-attribute 'italic nil :slant 'normal)

(setopt global-hl-line-sticky-flag t
        cursor-type '(bar . 1)
        truncate-string-ellipsis " …")
(setopt mode-line-compact t
        mode-line-collapse-minor-modes t
        mode-line-modes-delimiters '("[" . "]"))

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(blink-cursor-mode -1)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config (setopt display-line-numbers-type 'visual
                  display-line-numbers-grow-only t
                  display-line-numbers-width-start t))

(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))

(use-package paren
  :ensure nil
  :init (show-paren-mode 1)
  :config (setopt show-paren-delay 0.0125
                  show-paren-context-when-offscreen t
                  show-paren-not-in-comments-or-strings 'on-mismatch))

(use-package paren-face
  :ensure t
  :hook (prog-mode . paren-face-mode)
  :config (set-face-attribute 'parenthesis nil :foreground "#989898"))

;; (use-package highlight-parentheses
;;   :ensure t
;;   :hook (prog-mode . highlight-parentheses-mode))

;; UTF-8 is enough for modern Emacs
;; (set-language-environment 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; (set-keyboard-coding-system 'utf-8-unix)
(setq-default default-input-method nil)

(set-face-attribute 'default nil
                    :family "Tempestypes"
                    :width 'condensed
                    :height 140)

(let ((font "PingFang SC"))
  (set-fontset-font t 'kana (font-spec :family font))
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))

(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(provide 'init-gui-frames)
