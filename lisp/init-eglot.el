;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Eglot setup
(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  :config
  (setq read-process-output-max (* 1024 1024))
  :bind
  ((:map eglot-mode-map
         ("s-i" . 'eglot-format-buffer))))


;;; Syntax highlighting
(use-package tree-sitter
  :init
  (setq treesit-extra-load-path
        (expand-file-name "treesit/dist/" user-emacs-directory))
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :after tree-sitter)


;;; C/C++ support
(require 'cc-mode)

(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)

(add-hook 'c++-mode-hook 'eglot-ensure)

(defun my/compile--cc-file ()
  (interactive)
  (compile (format "g++-12 -o %s %s -g -lm -Wall"
                   (file-name-sans-extension (buffer-name))
                   (buffer-name))))
(define-key c++-mode-map [f9] 'my/compile--cc-file)


;;; Python support
;; Note since `run-python' command is set globally,
;; we will not use `:defer' in this module
;; Enable `eglot' support
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package python-mode
  :straight (:type built-in)
  :defer t
  :init

  ;; Python executable file location
  (setq-default python-shell-interpreter my-python-exec-path)
  (with-eval-after-load 'org
    ;; Language specified settings
    (setq-default org-babel-python-command my-python-exec-path))

  ;; Ignore the warnings
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  :bind
  (("M-p r" . run-python)))




(provide 'init-eglot)
;;; init-eglot.el ends here
