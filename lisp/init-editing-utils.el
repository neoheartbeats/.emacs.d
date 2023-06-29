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
(add-hook 'after-init-hook #'(lambda ()
                               (electric-pair-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook (after-init . global-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Framework for mode-specific buffer indexes
(use-package imenu :bind ("s-m" . imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook #'(lambda ()
                                (rainbow-delimiters-mode 1)))
  (add-hook 'text-mode-hook #'(lambda ()
                                (rainbow-delimiters-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(global-display-fill-column-indicator-mode 1)

;; Display line numbers
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))


(provide 'init-editing-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-editing-utils.el ends here
