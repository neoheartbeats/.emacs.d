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
  :demand t
  :config

  ;; Mapping colors
  (setq modus-themes-common-palette-overrides
        `((cursor red)

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

;; (use-package col-highlight
;;   :load-path "~/.emacs.d/site-lisp/col-highlight/"
;;   :config
;;   (custom-set-faces
;;    '(col-highlight ((t (:background "#1e1e1e")))))
;;   (add-hook 'text-mode-hook #'(lambda ()
;;                                 (column-highlight-mode 1)))
;;   (add-hook 'prog-mode-hook #'(lambda
;;                                 (column-highlight-mode 1))))

;;; Add paddings

(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))

;;; Mode line

(setq-default mode-line-compact t)

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
;; Contrary to what many Emacs users have in their configs, you don't need more than
;; this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is a step
;; too opinionated.
(setq default-input-method nil)


;;; Font settings

(set-face-attribute 'default nil :family "Tempestypes" :height 140 :weight 'light)
(set-face-attribute 'bold nil :family "Tempestypes" :weight 'regular)

;; No need for italic fonts
(set-face-italic 'italic nil)

;; Set up font for non-ascii fontset
(set-fontset-font t 'big5                (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'big5-hkscs          (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-1  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-15 (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-2  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-3  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-4  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-5  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-6  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-cns11643-7  (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-gb2312      (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'chinese-gbk         (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'kanbun              (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'bopomofo            (font-spec :family "Noto Serif CJK SC"))
(set-fontset-font t 'han                 (font-spec :family "Noto Serif CJK SC"))

(set-fontset-font t 'japanese-jisx0208        (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'japanese-jisx0208-1978   (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'japanese-jisx0212        (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'japanese-jisx0213-1      (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'japanese-jisx0213-2      (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'japanese-jisx0213.2004-1 (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'jisx0201                 (font-spec :family "Noto Serif CJK JP"))
(set-fontset-font t 'kana                     (font-spec :family "Noto Serif CJK JP"))

(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'append)
(set-fontset-font t 'ucs   (font-spec :family "SF Pro") nil 'append)

;; Define the ligation dictionary
(defun sthenno/ligation-setup ()
  (let ((composition-table (make-char-table nil)))
    (dolist
        (char-regexp-replacement
         `((33  . ".\\(?:\\(==\\|!==\\|!=\\|!===\\)\\|[!=]\\)")
           (35  . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
           (40  . ".\\(?:(\\*\\|[(*]\\)")
           (41  . ".\\(?:\\*)\\|[)*]\\)")
           (42  . ".\\(?:\\*/\\|\\*+\\|\\*>\\|<\\*>\\|<\\*\\|=\\*\\|\\+=\\)")
           (43  . ".\\(?:\\+\\+\\|\\+\\+\\+\\)")
           (45  . ,(concat ".\\(?:-->\\|->\\|->>\\|--\\|---\\|->-\\|<-\\|<--\\|"
                           "<---\\|<<-\\|-<\\|-<<\\|<->\\|<-->\\|<--->\\|"
                           "<---->\\||-\\|-\\|[<>|~-]\\)"))
           (46  . ".\\(?:\\.[.<]\\|<\\.\\|<\\.[>]\\|\\.>\\|[.:<-]\\)")
           (47  . ".\\(?:\\*/\\|//\\|/\\\\\\|\\\\/\\|/>\\|</\\|</>\\|[*/>]\\)")
           (58  . ".\\(?::::\\|::\\|:=\\|<:\\|=:\\|:>\\|[:]\\)")
           (60  . ,(concat ".\\(?:<!--\\|<!---\\|<--\\|<---\\|<----\\|<<-\\|<-\\|"
                           "<=\\|<==\\|<===\\|<====\\|<<=\\|=<\\|=<<\\|<=>\\|"
                           "<==>\\|<===>\\|<====>\\|<>\\|<\\*\\|<\\*>\\|<|\\|"
                           "<|>\\|<\\.\\|<\\.[>]\\|[<|.-]\\)"))
           (61  . ,(concat ".\\(?:=>\\|=>>\\|==>\\|===>\\|=>-\\|==\\|===\\|!==\\|"
                           "!===\\|<=>\\|<==>\\|<===>\\|<====>\\|=<\\|=<<\\|"
                           "=<=\\|=\\*\\|[=>]\\)"))
           (62  . ,(concat ".\\(?:->\\|->>\\|-->\\|--->\\|->-\\|>>\\|>-\\|>>-\\|"
                           ">=\\|>==\\|>===\\|>>=\\|\\*>\\||>\\|\\.>\\|"
                           "[>=.-|]\\)"))
           (63  . ".\\(?:\\?\\?\\|[?]\\)")
           (91  . ".\\(?:\\[|\\|[\\[]\\)")
           (92  . ".\\(?:\\\\\\\\\\|\\\\/\\|[\\\\]\\)")
           (93  . ".\\(?:|]\\|[\\]]\\)")
           (95  . ".\\(?:__\\|[_]\\)")
           (123 . ".\\(?:{|\\|[{]\\)")
           (124 . ".\\(?:<|>\\||>\\|<|\\||]\\|\\[|\\|{|\\||\\}\\||\\-\\|\\-|\\|[|]\\)")
           (125 . ".\\(?:|\\}\\|[}]\\)")
           (126 . ".\\(?:<~~\\|~~>\\|[~]\\)")))
      (set-char-table-range composition-table (car char-regexp-replacement)
                            `([,(cdr char-regexp-replacement) 0 font-shape-gstring])))
    (set-char-table-parent composition-table composition-function-table)
    (setq composition-function-table composition-table)))
(add-hook 'after-init-hook #'sthenno/ligation-setup)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1 :weight 'thin)

;; Ellipsis symbol
(setq truncate-string-ellipsis " …")

(provide 'init-gui-frames)

;;; init-gui-frames.el ends here
