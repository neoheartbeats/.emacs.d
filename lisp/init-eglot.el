;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-autoshutdown t)
  :config
  (define-key eglot-mode-map (kbd "s-i") 'eglot-format-buffer)
  :hook
  ((c-mode c++-mode python-mode LaTeX-mode) . eglot-ensure))

(use-package consult-eglot)


(require 'cc-mode)

(setq-default c-default-style "gnu")
(setq-default c-basic-offset 4)

;; Using Homebrew's version of `clangd'
;; (defvar my/homebrewed-clangd "/opt/homebrew/opt/llvm/bin/clangd")

(defun my/compile--cc-file ()
  (interactive)
  (compile (format "g++-12 -o %s %s -g -lm -Wall"
                   (file-name-sans-extension (buffer-name))
                   (buffer-name))))
(define-key c++-mode-map [f9] 'my/compile--cc-file)



(provide 'init-eglot)
;;; init-eglot.el ends here
