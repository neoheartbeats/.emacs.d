;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
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
;; Misc settings
(setq undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb
(setq truncate-string-ellipsis "􀍠")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Delete selection if you insert
(use-package delsel :hook (after-init . delete-selection-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Text replacement
(use-package replace :bind ("<f10>" . query-replace))

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
(use-package rainbow-delimiters
  :ensure t
  :diminish (rainbow-delimiters-mode)
  :config
  (add-hook 'prog-mode-hook #'(lambda ()
                                (rainbow-delimiters-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(add-hook 'after-init-hook #'(lambda ()
                               (global-display-fill-column-indicator-mode 1)))

;; Display line numbers
(setq display-line-numbers-width-start t)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))

(provide 'init-editing-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-editing-utils.el ends here
