;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file mainly includes an Emacs UI design optimized for GUI mode. This file mainly
;; configures `modus-themes', fonts, some global `face' and `mode-line' styles. For
;; those feature-specific `faces', their configuration code is isolated. A typical
;; example is `prettify-symbols-mode'. See also: `init-editing-utils', `init-org'.
;;

;;; Code:

;;; Modus Themes

(use-package modus-themes
  :ensure t
  :config

  ;; Mapping colors
  (setq modus-themes-common-palette-overrides
        `(
          ;; Make the mode line borderless
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; Set color faces for `display-line-numbers-mode'
          (fg-line-number-active fg-dim)
          (bg-line-number-active bg-hl-line)
          (fg-line-number-inactive "#535353")
          (bg-line-number-inactive unspecified)

          ;; Subtle underlines
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)

          ;; Make links the same color as `fg-main'
          ;; This also affects `button' faces in Modus Themes
          (fg-link unspecified)
          (fg-link-visited unspecified)

          ;; Prose colors
          (prose-todo info)
          (prose-done "#535353")

          ;; Matching parenthesis
          (fg-paren-match green)
          (bg-paren-match unspecified)

          ;; Make code blocks more minimal
          (bg-prose-block-contents  unspecified)
          (bg-prose-block-delimiter unspecified)
          (fg-prose-block-delimiter fg-dim)

          ;; Completions (see also `init-comp')
          (bg-completion bg-hl-line)

          ;; Apply the presets
          ,@modus-themes-preset-overrides-faint))

  ;; Load the enable the theme
  (modus-themes-load-theme 'modus-vivendi))

(custom-set-faces
 '(region ((t (:extend nil)))))

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode 1)

;;; Add paddings
(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))

;;; Mode line

(setq-default mode-line-compact t)

;; Display current function

;; (add-hook 'after-init-hook #'(lambda ()
;;                                (which-function-mode 1)))
;; (setq which-func-update-delay 0.125)

;; Diminish some built-in modes

(use-package eldoc :diminish)

;;; Parentheses

(use-package paren
  :config
  (custom-set-faces
   '(show-paren-match ((t :inherit 'bold))))
  (show-paren-mode 1))

;; Dim parenthesis for s-expressions
(use-package paren-face
  :ensure t
  :demand t
  :config
  (custom-set-faces
   '(parenthesis ((t (:foreground "#535353")))))
  (global-paren-face-mode 1))

;; Hightlight parentheses dynamically surrounding point
(use-package highlight-parentheses
  :ensure t
  :diminish
  :config (global-highlight-parentheses-mode 1))

;;; Cursor faces

(setopt cursor-type '(bar . 1))
(setopt mouse-highlight nil)

(blink-cursor-mode -1)

;;; Encodings

;; Contrary to what many Emacs users have in their configs, you don‘t need more than
;; this to make UTF-8 the default coding system:
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
;; ...but `set-language-environment' also sets `default-input-method', which is a step
;; too opinionated.
(setq default-input-method nil)

;;; Font settings

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
;; Line spacing
;; (setq-default line-spacing 0.25)

;; No need for italic fonts
;; (set-face-italic 'italic nil)

;; Set up font for non-ascii fontset
(set-fontset-font t 'han (font-spec :family "Songti SC"))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

;; Typographic ligatures

(use-package ligature
  :ensure t
  :config (let* ((ligs '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
                         "<---->" "<!--"
                         "<==" "<===" "<=" "=>" "==>" "===>" ">=" "<=>" "<==>" "<===>"
                         "<====>" "<!---"
                         "::" ":::" "==" "!=" "===" "!=="
                         ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:"
                         "<******>" "++" "+++"))
                 (ligs-dict (append '("__" "--") ligs))
                 (ligs-text (append '("__")      ligs)))
            (ligature-set-ligatures 'prog-mode ligs-dict)
            (ligature-set-ligatures 'text-mode ligs-text)

            ;; Enables ligature checks globally in all buffers
            (global-ligature-mode 1)))

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1 :weight 'thin)

;; Ellipsis symbol
(setq truncate-string-ellipsis " …")

(provide 'init-gui-frames)

;;; init-gui-frames.el ends here
