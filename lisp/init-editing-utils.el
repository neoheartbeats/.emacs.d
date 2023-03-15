;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file is inspired by https://github.com/purcell/emacs.d/.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Electric parentheses
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-pair-mode))
(add-hook 'after-init-hook #'electric-indent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Delete selection if you insert
(use-package delsel
  :hook
  (after-init . delete-selection-mode))

;; Smart deletion
(use-package smart-hungry-delete :ensure t
  :init
  (smart-hungry-delete-add-default-hooks)
  :bind
  (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	([remap delete-backward-char] . smart-hungry-delete-backward-char)
	([remap delete-char] . smart-hungry-delete-forward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Framework for mode-specific buffer indexes
(use-package imenu
  :bind
  (("s-m" . imenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using rainbow delimiters
(use-package rainbow-delimiters :ensure t :demand t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Display line numbers
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(provide 'init-editing-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-editing-utils.el ends here
