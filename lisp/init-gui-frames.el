;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file mainly includes an Emacs UI design optimized for GUI mode. This file mainly
;; configures `modus-themes', fonts, some global `face' and `mode-line' styles.
;; For those feature-specific `faces', their configuration code is isolated. A typical
;; example is `prettify-symbols-mode'. See also: `init-editing-utils', `init-org'.
;;
;;; Code:
;;

;; Modus Themes
(use-package modus-themes
  :straight t
  :config

  ;;; `modus-vivendi' customizations
  ;;
  ;; Define a user palette (for Sthenno)
  ;;

  (setq modus-vivendi-palette-user
        '((sthenno-blueberry "#467fef")))

  ;; Mapping colors
  (setq modus-vivendi-palette-overrides
        '(
          ;; Make the mode-line borderless and stand out less
          (bg-mode-line-active bg-active)
          (fg-mode-line-active fg-main)
          (bg-mode-line-inactive bg-main)
          (fg-mode-line-active fg-dim)

          ;; Make the mode line borderless
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; Set color faces for `display-line-numbers-mode'
          (fg-line-number-active fg-main)
          (bg-line-number-active bg-hl-line)
          (fg-line-number-inactive fg-dim)
          (bg-line-number-inactive unspecified)

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

          ;; Prose colors
          (prose-todo info)
          (prose-done fg-dim)

          ;; Custom region colors
          (fg-region unspecified)
          (bg-region bg-paren-match)

          ;; Make code blocks more minimal
          (bg-prose-block-contents unspecified)
          (bg-prose-block-delimiter unspeficied)
          (fg-prose-block-delimiter fg-dim)


          ;; Completions (see also `init-comp')
          (bg-completion bg-hl-line)

          (cursor sthenno-blueberry)))

  (setq modus-themes-bold-constructs t
        modus-themes-prompts '(bold)
        modus-themes-completions '((t . (bold))))

  ;; Enable and load the theme
  (modus-themes-load-theme 'modus-vivendi))

;; Do not extend `region' background past the end of the line
(custom-set-faces '(region ((t :extend nil))))


;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;; Cursor faces
(setq-default cursor-type '(bar . 1))
(setq blink-cursor-mode nil)

;; highlight current line
(add-hook 'after-init-hook #'(lambda ()
                               (global-hl-line-mode 1)))

;; Custom font
(set-face-attribute 'default nil :family "Sthenno Mono" :height 140)

;; Define the ligation dictionary [FIXME]
(defun sthenno-mono-ligation-setup ()
  (let ((composition-table (make-char-table nil)))
    (dolist (char-regexp-replacement
             '((33 . ".\\(?:\\(==\\|!=\\)\\|[!=]\\)") ; != ==
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)") ; ### ## #( #_
               (45 . ".\\(?:->\\|--\\|[<>|~-]\\)") ; -> -- >>
               (46 . ".\\(?:\\.[.<]\\|[.:<-]\\)") ; .. .< .:
               (47 . ".\\(?:\\*/\\|//\\|[*/>]\\)") ; */ // /->
               (58 . ".\\(?:::\\|[:=]\\)") ; :: :=
               (60 . ".\\(?:!--\\|--\\|<[<=|-]\\|=[<=|-]\\|<=\\)") ; <!-- << <- <=
               (61 . ".\\(?:\\|=[=>]\\|=>\\)") ; == =>
               (62 . ".\\(?:>>\\|>[=>-]\\|>=\\)") ; >> >=
               (63 . ".\\(?:\\?\\?\\|[:=?]\\)") ; ?? ?: ?
               (92 . ".\\(?:\\\\\\\\\\|[\\n]\\)"))) ; \\ \n
      (set-char-table-range composition-table (car char-regexp-replacement)
                            `([,(cdr char-regexp-replacement) 0 font-shape-gstring])))

    (set-char-table-parent composition-table composition-function-table)
    (setq composition-function-table composition-table)))

(add-hook 'after-init-hook #'sthenno-mono-ligation-setup)

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
;;
;; Do not display line numbers in mode line
;;

(setq line-number-mode nil)

;; Removal of mule-info, and a trim of all double-spaces anywhere in the mode line
;; format to a single space
;; From https://github.com/jdtsmith/mlscroll
(setq-default mode-line-format
              (cl-nsubst-if " " (lambda (x)
                                  (and (stringp x) (string-blank-p x) (> (length x) 1)))
                            (remove 'mode-line-mule-info mode-line-format)))

;; MLScroll: A scrollbar for the Emacs mode line
(use-package mlscroll
  :straight t
  :config
  (setq mlscroll-width-chars 15
        mlscroll-minimum-current-width 1)

  (modus-themes-with-colors
    (setq mlscroll-in-color bg-active
          mlscroll-out-color fg-dim))

  (mlscroll-mode 1))

;;
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
(use-package spacious-padding
  :straight t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 0
           :mode-line-width 4
           :tab-width 4
           :right-divider-width 0
           :scroll-bar-width 0
           :fringe-width 10))

  (spacious-padding-mode 1))


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
