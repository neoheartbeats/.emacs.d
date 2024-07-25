;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t; -*-

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

;;
;; Modus Themes
;;

(use-package modus-themes
  :ensure t
  :config

  ;; `modus-vivendi' customizations
  ;;
  ;; Define a user palette (for Sthenno)
  ;;

  (setq modus-vivendi-palette-user
        '((sthenno-blueberry "#467fef")
          (sthenno-gorse     "#fcf251")))

  ;; Mapping colors
  ;;
  (setq modus-vivendi-palette-overrides
        `((bg-main "#0e1116")
          (fg-main "#e7edf2")
          
          ;; Make the mode-line borderless and stand out less
          (bg-mode-line-active   bg-active)
          (fg-mode-line-active   fg-main)
          (bg-mode-line-inactive bg-dim)
          (fg-mode-line-inactive fg-dim)

          ;; Make the mode line borderless
          (border-mode-line-active   unspecified)
          (border-mode-line-inactive unspecified)

          ;; Set color faces for `display-line-numbers-mode'
          (fg-line-number-active   fg-main)
          (bg-line-number-active   bg-hl-line)
          (fg-line-number-inactive fg-dim)
          (bg-line-number-inactive unspecified)

          ;; Make the fringe invisible
          (fringe unspecified)

          ;; Subtle underlines
          (underline-link          border)
          (underline-link-visited  border)
          (underline-link-symbolic border)

          ;; Make links the same color as `fg-main'
          ;; This also affects `button' faces in Modus Themes
          (fg-link         unspecified)
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

          (cursor sthenno-blueberry)

          ,@modus-themes-preset-overrides-warmer))

  ;; Enable and load the theme
  (modus-themes-load-theme 'modus-vivendi))

;; Do not extend `region' background past the end of the line
(custom-set-faces '(region ((t :extend nil))))


;; Clean up the title bar content
(setq-default frame-title-format nil)
(setq-default ns-use-proxy-icon nil)

(progn

  ;; No need to echo
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil))

;; Cursor faces
(setq-default cursor-type '(bar . 1))
(blink-cursor-mode -1)
(setq mouse-highlight nil)

;; highlight current line
(add-hook 'after-init-hook #'(lambda ()
                               (global-hl-line-mode 1)))

;; Custom font
(set-face-attribute 'default     nil :family "Sthenno Mono" :height 140)
(set-face-attribute 'fixed-pitch nil :family "Sthenno Mono" :height 140)

;; Define the ligation dictionary
(defun sthenno-mono-ligation-setup ()
  (let ((composition-table (make-char-table nil)))
    (dolist (char-regexp-replacement
             '((33 . ".\\(?:\\(==\\|!=\\)\\|[!=]\\)")              ; != ==
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")          ; ### ## #( #_
               (45 . ".\\(?:->\\|--\\|[<>|~-]\\)")                 ; -> -- >>
               (46 . ".\\(?:\\.[.<]\\|[.:<-]\\)")                  ; .. .< .:
               (47 . ".\\(?:\\*/\\|//\\|[*/>]\\)")                 ; */ // /->
               (58 . ".\\(?:::\\|[:=]\\)")                         ; :: :=
               (60 . ".\\(?:!--\\|--\\|<[<=|-]\\|=[<=|-]\\|<=\\)") ; <!-- << <- <=
               (61 . ".\\(?:\\|=[=>]\\|=>\\)")                     ; == =>
               (62 . ".\\(?:>>\\|>[=>-]\\|>=\\)")                  ; >> >=
               (63 . ".\\(?:\\?\\?\\|[:=?]\\)")                    ; ?? ?: ?
               (92 . ".\\(?:\\\\\\\\\\|[\\n]\\)")))                ; \\ \n
      (set-char-table-range composition-table (car char-regexp-replacement)
                            `([,(cdr char-regexp-replacement) 0 font-shape-gstring])))

    (set-char-table-parent composition-table composition-function-table)
    (setq composition-function-table composition-table)))

(add-hook 'after-init-hook #'sthenno-mono-ligation-setup)

;; Set up font for unicode fontset
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

(set-fontset-font t 'unicode (font-spec :family "SF Pro Display")  nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)

;; Note this make all italic font style disabled
(set-face-attribute 'italic nil :slant 'normal)

;; Make `fill-column-indicator' thinner
(set-face-attribute 'fill-column-indicator nil :height 0.1)

;; Stop showing fringe bitmaps
;; (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;; Ellipsis symbol
(setq truncate-string-ellipsis " 􀍠")

;; TODO:
(add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function)


;; Collect pop-up windows
;;
(use-package popper
  :ensure t
  :defer t
  :init
  (setq popper-reference-buffers '("\\*ielm\\*$"
                                   "\\*Messages\\*$"
                                   "\\*scratch\\*$"

                                   ;; Modes
                                   snippet-mode
                                   help-mode))

  ;; Disable the modeline for `popper' buffers
  (setq popper-mode-line '(:eval (propertize " 􁐈 " 'face 'mode-line-emphasis)))

  ;; Hooks
  (add-hook 'after-init-hook #'(lambda ()
                                 (popper-mode 1)))
  
  :bind (:map global-map
              ("C-p" . popper-toggle)))


;;; Mode Line settings
;;
;; Format modeline buffer identification
;;
;; (defun sthenno/modeline-buffer-identification-setup ()
;;   (defun sthenno/modeline--buffer-identification ()
;;     "Return `buffer-name' as a string.
;; If `buffer-file-name' is a `denote' file, return its corresponding title instead."
;;     (let ((file (buffer-file-name)))
;;       (if (denote-file-is-note-p file)
;;           (format "[denote:%s]" (denote-retrieve-filename-title (buffer-file-name)))
;;         (buffer-name))))

;;   (defvar-local sthenno/modeline-buffer-identification
;;       '(:eval
;;         (when (mode-line-window-selected-p)
;;           (propertize (sthenno/modeline--buffer-identification)
;;                       'face 'mode-line-buffer-id)))
;;     "Mode line construct to display the buffer identification.")

;;   (defvar-local sthenno/modeline-prefix-symbol
;;       (modus-themes-with-colors
;;         (propertize "  Sthenno ◈" 'face
;;                     `(:foreground ,sthenno-gorse :inherit mode-line-buffer-id))))

;;   (setq-default mode-line-format `(,sthenno/modeline-prefix-symbol
;;                                    "  "
;;                                    ,sthenno/modeline-buffer-identification
;;                                    "  "
;;                                    mode-line-modes
;;                                    mode-line-misc-info
;;                                    mode-line-end-spaces)))

;; (add-hook 'after-init-hook #'sthenno/modeline-buffer-identification-setup)
(setq mode-line-compact t)

;; TODO
;; (use-package minions
;;   :ensure t
;;   :init (setq minions-mode-line-lighter "􀠩")
;;   :config (minions-mode 1))


;; Automatic adjusting for margins
(use-package spacious-padding
  :ensure t
  :init
  (setq spacious-padding-widths '( :internal-border-width 15
                                   :header-line-width 0
                                   :mode-line-width 4
                                   :right-divider-width 0
                                   :scroll-bar-width 0))
  
  :config (spacious-padding-mode 1))


;;; Sthenno's customization of "doom-quit"
;;
;; See https://github.com/doomemacs/doomemacs/tree/master/modules/ui/doom-quit
;;
(defvar sthenno-quit-messages  `("Anyone else but you?"
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
   (format "Sthenno: %s 􀄫 %s"
           (propertize (nth (random (length sthenno-quit-messages))
                            sthenno-quit-messages)
                       'face 'modus-themes-prompt)
           "Really quit Emacs?")))

(setq confirm-kill-emacs #'sthenno-quit-fn)

(provide 'init-gui-frames)
