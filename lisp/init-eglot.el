;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup `treesit'
(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode 1))

;; Remap modes to use `treesit'
(setq major-mode-remap-alist '((python-mode . python-ts-mode)))

;; Initialize `eglot'
(use-package eglot :ensure t
  :config
  (add-hook 'python-ts-mode-hook #'eglot-ensure))


(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
