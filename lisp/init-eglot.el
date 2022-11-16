;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Eglot setup
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (setq read-process-output-max (* 1024 1024))
  (define-key eglot-mode-map (kbd "s-i") 'eglot-format-buffer))


;;; C/C++ support
(require 'cc-mode)

(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Using Homebrew's version of `clangd'
(add-to-list 'eglot-server-programs
             '((c-mode c++-mode) . ("/opt/homebrew/opt/llvm/bin/clangd")))

(defun my/compile--cc-file ()
  (interactive)
  (compile (format "g++-12 -o %s %s -g -lm -Wall"
                   (file-name-sans-extension (buffer-name))
                   (buffer-name))))
(define-key c++-mode-map [f9] 'my/compile--cc-file)


;;; Python support
;; Python executable file location
(setq org-babel-python-command "python3.10")
(setq python-shell-interpreter "python3.10")

;; Ignore the warnings
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Enable `eglot' support
(add-hook 'python-mode-hook 'eglot-ensure)


(provide 'init-eglot)
;;; init-eglot.el ends here
