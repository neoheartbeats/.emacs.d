;; init-lsp.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Language server protocol support for Emacs.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup LSP core
;;
;; Setup `lsp-mode'
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook
  ;; Enable `which-key' integration
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; Cleanup blacklists
  (lsp-workspace-blacklist-remove "~/org/")
  :commands lsp)

;; Setup `lsp-ui'
(use-package lsp-ui
  :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load language servers
;;
;; Python
(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp))))

;; LaTeX
(use-package lsp-latex
  :hook
  (tex-mode . lsp)
  (latex-mode . lsp)
  (bibtex-mode . lsp))

(provide 'init-lsp)
