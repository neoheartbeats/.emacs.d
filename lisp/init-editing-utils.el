;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Electric parenthesis
;;
(add-hook 'after-init-hook #'(lambda ()
                               (electric-pair-mode 1)))

;; Highlight parenthesis matched off-screen (Emacs 30+)
;; (setopt blink-matching-paren-highlight-offscreen t)

;;
;; Misc settings
;;
(setopt undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb
(setopt truncate-string-ellipsis "")

;;
;; Delete selection if you insert
;;
(use-package delsel
  :hook (after-init . delete-selection-mode))

;;
;; Automatically reload files was modified by external program
;;
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook (after-init . (lambda ()
                        (global-auto-revert-mode 1))))

;;
;; Framework for mode-specific buffer indexes
;;
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
(setopt display-fill-column-indicator-character ?\u254e)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-fill-column-indicator-mode 1)))

;; Display line numbers
(add-hook 'text-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))

(setq-default display-line-numbers-width 5)

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
