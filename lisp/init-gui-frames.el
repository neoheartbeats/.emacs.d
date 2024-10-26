;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

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
  :vc (modus-themes
       :url "https://gitlab.com/protesilaos/modus-themes"
       :branch "main")
  :demand t
  :config
  (setopt modus-themes-bold-constructs t
          modus-themes-italic-constructs t)

  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  
  ;; Mapping colors
  (setopt modus-vivendi-palette-overrides
          '(
            ;; Make the mode-line borderless and stand out less
            (bg-mode-line-active bg-active)
            (fg-mode-line-active fg-main)
            (bg-mode-line-inactive bg-dim)
            (fg-mode-line-inactive fg-dim)

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
            (underline-link-visited  border)
            (underline-link-symbolic border)

            ;; Make links the same color as `fg-main'
            ;; This also affects `button' faces in Modus Themes
            (fg-link unspecified)
            (fg-link-visited unspecified)

            ;; Prose colors
            (prose-todo info)
            (prose-done fg-dim)

            ;; Matching parenthesis
            (fg-paren-match fg-main)
            (bg-paren-match bg-green-intense)

            ;; Custom region colors
            (fg-region unspecified)
            (bg-region bg-cyan-subtle)

            ;; Make code blocks more minimal
            (bg-prose-block-contents unspecified)

            ;; Completions (see also `init-comp')
            (bg-completion bg-hl-line)

            ;; Org-mode headings
            ;; (fg-heading-1 magenta)

            ;; Cursor color. See also `cursor-type'
            (cursor red-faint)))

  ;; Enable and load the theme
  (modus-themes-load-theme 'modus-vivendi))

;; Do not extend `region' background past the end of the line
(custom-set-faces '(region ((t :extend nil))))

;; Remove the `underline' for agenda views
(custom-set-faces '(org-agenda-date-today ((t :underline nil))))

;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

;;; Highlight current line
;; (global-hl-line-mode 1)
(add-hook 'prog-mode-hook #'(lambda ()
                              (hl-line-mode 1)))

;; Encodings
;;
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)

;; Custom font
(set-face-attribute 'default nil :family "Triplicate A Code" :height 140)

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

(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs   (font-spec :family "SF Pro") nil 'prepend)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1)

;; Ellipsis symbol
(setq truncate-string-ellipsis " 􀍠")

;; Text quoting
(setopt text-quoting-style 'straight)

;; Do not show buffer boundaries
(setq indicate-buffer-boundaries nil)

;; Do not show empty lines
(setq indicate-empty-lines nil)

;;; Mode Line settings
(setopt mode-line-compact t)

;; (use-package minions
;;   :ensure t
;;   :init (setq minions-mode-line-lighter "􀠩")
;;   :config (minions-mode 1))

;; (setq-default mode-line-format '("%e"
;;                                  " "
;;                                  sthenno/mode-line-buffer
;;                                  " "
;;                                  sthenno/mode-line-major))

;; (defun sthenno/mode-line--buffer ()
;;   (let ((buff (if (denote-file-is-note-p (buffer-file-name))
;;                   (denote-retrieve-filename-title (buffer-file-name))
;;                 (buffer-name))))
;;     (format "%s" buff)))

;; (defvar-local sthenno/mode-line-buffer
;;     '(:eval
;;       (list (format "BUFF: %s"
;;                     (propertize (sthenno/mode-line--buffer) 'face 'success))))
;;   "Mode-line construct to display the current buffer.")
;; (put 'sthenno/mode-line-buffer 'risky-local-variable t)

;; (defvar-local sthenno/mode-line-major
;;     '(:eval
;;       (format "MODE: %s"
;;               (propertize (symbol-name major-mode)) 'face 'default))
;;   "Mode-line construct to display the Major.")
;; (put 'sthenno/mode-line-major 'risky-local-variable t)


;; Automatic adjusting for margins
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :config (spacious-padding-mode 1))


;; Sthenno's customization of "doom-quit"
;;
;; See https://github.com/doomemacs/doomemacs/tree/master/modules/ui/doom-quit
;;
;; (defvar sthenno-quit-messages '("Anyone else but you?"
;;                                 "She depends on you."
;;                                 "Please take care of Sthenno."
;;                                 "It's not like I'll miss you or anything, b-baka!"
;;                                 "Please don't go!")
;;   "A list of quit messages, picked randomly by `sthenno-quit'.")

;; (defun sthenno-quit-p (&optional prompt)
;;   "Prompt the user for confirmation when killing Emacs.
;; Returns t if it is safe to kill this session. Does not prompt if no real buffers
;; are open."
;;   (or (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
;;       (ignore (message "Aborted"))))

;; (defun sthenno-quit-fn (&rest _)
;;   (sthenno-quit-p
;;    (format "Sthenno: %s 􀄫 %s"
;;            (propertize (nth (random (length sthenno-quit-messages))
;;                             sthenno-quit-messages)
;;                        'face 'modus-themes-prompt)
;;            "Really quit Emacs?")))

;; (setq confirm-kill-emacs #'sthenno-quit-fn)

(provide 'init-gui-frames)
