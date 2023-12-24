;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file complement the develop environment for specific languages.
;;

;;; Code:

;;
;; Setup `treesit' for better performance for processing coding syntax
;;

;;;
;; Command `treesit-auto-install-all' is required if the tree-sitter grammar
;; libs have not been configured already
(use-package treesit-auto
  :straight t
  :config (global-treesit-auto-mode 1))

;;;
;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
(setq treesit-font-lock-level 4)

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;
;; Initialize `eglot'
;;
(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-hook 'python-mode #'eglot-ensure))

;;
;; Python
;;
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-enable nil)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Reformat python buffers using the `black' formatter
;; (use-package blacken
;;   :straight t
;;   :config

;;   ;; Auto reformat the buffer after saving
;;   (add-hook 'python-mode-hook #'(lambda ()
;;                                      (blacken-mode 1)))
;;   :bind
;;   (:map python-mode-map
;;         ("s-i" . blacken-buffer)))

;;
;; AI
;;

;;
;; GitHub Copilot
;;
;; (use-package copilot
;;   :straight (
;;              :host github
;;              :repo "zerolfx/copilot.el"
;;              :files ("dist" "*.el"))
;;   :config
;;   (add-hook 'prog-mode-hook #'(lambda ()
;;                                 (copilot-mode 1)))
;;   (define-key copilot-completion-map (kbd "M-.") #'copilot-accept-completion))

;;;
;;
;; GPTel: A simple LLM client for Emacs
;;
;; (use-package gptel
;;   :straight t
;;   :config
;;   (setq-default gptel-model "neural-chat:latest"
;;                 gptel-backend (gptel-make-ollama
;;                                "Sthenno"
;;                                :host "127.0.0.1:11434"
;;                                :models '("neural-chat:latest")
;;                                :stream t))
;;   (setq gptel-default-mode #'org-mode))

(provide 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
