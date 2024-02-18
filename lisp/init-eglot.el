;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

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

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;;
;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
(setq treesit-font-lock-level 4)

;;
;; Initialize `eglot'
;;
(use-package eglot
  :config

  ;; Use Pyright as the default language server
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-hook 'python-ts-mode #'eglot-ensure))

;; Auto confirm `.dir-locals.el' files
(setq-default enable-local-variables :safe)

;;
;; Python
;;
(setq python-interpreter "/opt/homebrew/bin/python3")
(setq org-babel-python-command python-interpreter)
(setq python-shell-interpreter python-interpreter)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-enable nil)

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Reformat python buffers using the `black' formatter
(use-package blacken
  :straight t
  :config (add-hook 'python-ts-mode-hook #'(lambda ()
                                          (blacken-mode 1)))
  :bind
  (:map python-ts-mode-map
        ("s-i" . blacken-buffer)))

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
;;   ;; (add-hook 'prog-mode-hook #'(lambda ()
;;   ;;                               (copilot-mode 1)))
;;   (define-key global-map (kbd "s-.") #'copilot-accept-completion))

(provide 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
