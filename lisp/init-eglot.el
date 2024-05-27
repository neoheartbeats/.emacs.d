;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

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
;; (use-package treesit-auto
;;   :straight t
;;   :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;;
;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
(setq treesit-font-lock-level 4)


;; Initialize `eglot'
;; (use-package eglot
;;   :config

;;   ;; Use Pyright as the default language server
;;   (add-to-list 'eglot-server-programs
;;                '(python-ts-mode . ("pyright-langserver" "--stdio")))
;;   (add-hook 'python-ts-mode #'eglot-ensure))

;; Auto confirm `.dir-locals.el' files
(setq-default enable-local-variables :safe)


;; Python
;; (setq python-interpreter
;;       "/opt/homebrew/Caskroom/miniconda/base/envs/develop/bin/python")
(setq org-babel-python-command python-interpreter)
(setq python-shell-interpreter python-interpreter)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-enable nil)

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Reformat python buffers using the `black' formatter
;; (use-package blacken
;;   :straight t
;;   :config (add-hook 'python-ts-mode-hook #'blacken-mode)
;;   :bind
;;   (:map python-ts-mode-map
;;         ("s-i" . blacken-buffer)))


;;; AI

;; GitHub Copilot
(use-package copilot
  :straight (
	     :host github
	     :repo "copilot-emacs/copilot.el"
	     :files ("*.el"))
  :config
  (define-key global-map (kbd "s-.") #'copilot-accept-completion))

;; GPTel: A simple LLM client for Emacs
;; (use-package gptel
;;   :straight t
;;   :config
;;   (setq-default gptel-model "phi3:latest"
;; 		gptel-backend (gptel-make-ollama "Phi-3"
;; 				:host "localhost:11434"
;; 				:stream t
;; 				:models '("phi3:latest")))
;;   (setq gptel-default-mode 'org-mode)
;;   (add-to-list 'gptel-directives '(explaining .
;; 					      "请使用中文翻译和简要解释输入的内容: "))
;;   :bind (("C-c g" . gptel))
;;   :hook (gptel-mode . visual-line-mode))


;;; EMMS
(use-package emms
  :straight t
  :config
  (emms-minimalistic)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/A55/")
  (setq emms-mode-line-icon-enabled-p 'nil)
  (add-hook 'emms-player-started-hook #'emms-shuffle))

(provide 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
