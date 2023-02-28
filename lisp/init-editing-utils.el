;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-indent-mode))
(setq-default indent-tabs-mode nil)

(require 'hl-defined)
(set-face-foreground 'hdefd-functions "#82b0ec")
(set-face-foreground 'hdefd-undefined "#fec43f")
(set-face-foreground 'hdefd-variables "#88ca9f")
(add-hook 'prog-mode-hook #'hdefd-highlight-mode)

;; Some basic preferences
(setq-default case-fold-search t)
(setq-default create-lockfiles nil)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default mark-even-if-inactive nil)
(setq-default mouse-yank-at-point t)
(setq-default ring-bell-function 'ignore)
(setq-default save-interprogram-paste-before-kill t)
(setq-default save-silently t)
(setq-default set-mark-command-repeat-pop t)
(setq-default sentence-end-double-space nil)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq-default use-short-answers t)
(setq-default help-window-select t)


;; Formatting files
;; Add a new line in the end of buffer while saving
(setq-default require-final-newline t)

;; Formatting elisp buffers
(use-package
 elisp-autofmt
 :custom
 (elisp-autofmt-on-save-p 'always)
 (elisp-autofmt-python-bin "/opt/homebrew/bin/python3")
 (elisp-autofmt-parallel-threshold 0) ; parallel computation for all buffers
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :bind ((:map emacs-lisp-mode-map (("s-i" . elisp-autofmt-buffer)))))


;; Enable the fundamental modes
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

;; Fill columns
(setq-default fill-column 80)
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

;; Deleting
(global-set-key (kbd "s-<backspace>") #'kill-whole-line)

;; Newline behaviours
(global-set-key (kbd "RET") #'newline-and-indent)

;; The nano style for truncated long lines
(setq auto-hscroll-mode 'current-line)
;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)
;; Display line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))
;; Enhance the performace of display
(setq display-raw-bytes-as-hex t)
(setq redisplay-skip-fontification-on-input t)

;; Use rainbow delimiters
(use-package
 rainbow-delimiters
 :demand t
 :config
 (add-hook 'prog-mode-hook #'(lambda () (rainbow-delimiters-mode 1))))

;; Always show the pointer's position
(setq-default make-pointer-invisible nil)
;; Hide cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)
;; Preserve contents of system clipboard
(setq-default save-interprogram-paste-before-kill t)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
