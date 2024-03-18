;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;; Electric parenthesis
(add-hook 'after-init-hook #'electric-pair-mode)

;; Highlight parenthesis matched off-screen
(setq blink-matching-paren-highlight-offscreen t)

;; Misc settings
(setq undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook (after-init . global-auto-revert-mode))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :bind ("s-m" . imenu))

;; Using rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :diminish (rainbow-delimiters-mode)
  :config (add-hook 'prog-mode-hook #'(lambda ()
                                        (rainbow-delimiters-mode 1))))

;; Inhibit paring these delimiters
(add-hook 'after-init-hook #'(lambda ()
                               (modify-syntax-entry ?< ".")))

;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode 1)

;; Draw horizontal rules for line-breaks
(use-package page-break-lines
  :straight t
  :config (global-page-break-lines-mode 1))

;; Display line numbers
(global-display-line-numbers-mode 1)

(setq-default display-line-numbers-width 5)

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
