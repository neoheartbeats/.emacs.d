;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 KAMUSUSANOWO

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:


;;
;; Electric parentheses
;;
(add-hook 'after-init-hook #'(lambda ()
                               (electric-pair-mode 1)))

;;
;; Misc settings
;;
(setq undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb
(setq truncate-string-ellipsis "􀍠")

;;
;; Delete selection if you insert
;;
(use-package delsel
  :hook (after-init . delete-selection-mode))

;;
;; Text replacement
;;
(use-package replace
  :bind ("<f10>" . query-replace))

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
  :config
  (add-hook 'prog-mode-hook #'(lambda ()
                                (rainbow-delimiters-mode 1))))

;; Inhibit paring these delimiters
(add-hook 'after-init-hook #'(lambda ()
                                    (modify-syntax-entry ?< ".")))

;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-fill-column-indicator-mode 1)))

;;
;; Improve deletion
;;
(use-package smart-hungry-delete
  :straight t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	       ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	       ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;; Display line numbers
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
