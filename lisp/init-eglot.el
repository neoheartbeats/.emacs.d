;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :straight (:type built-in))

(use-package consult-eglot)


(provide 'init-eglot)
;;; init-eglot.el ends here
