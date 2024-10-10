;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file complement the develop environment for specific languages.
;;

;;; Code:
;;

;;; Setup `treesit' for better performance for processing coding syntax
;;
;; Command `treesit-auto-install-all' is required if the tree-sitter grammar
;; libs have not been configured already
(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
;;
(setq treesit-font-lock-level 4)


;; Initialize `eglot'
(use-package eglot
  :ensure t
  :config

  ;; Hooks
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  
  :bind (:map eglot-mode-map
              ("<f6>" . eglot-rename)))

;; Automatically confirm .dir-locals.el files
(setq-default enable-local-variables :safe)


;; Python project management
(setopt python-indent-offset 4)

;; Reformat Python buffers using the Black formatter
(use-package blacken
  :ensure t
  :init (add-hook 'python-mode-hook #'(lambda ()
                                        (blacken-mode 1)))
  :bind (:map python-mode-map
              ("s-p" . blacken-buffer)))


;; JSON files
;; (add-hook 'json-ts-mode-hook #'(lambda ()
;;                                  (so-long-mode 1)))


;; GitHub Copilot
(use-package copilot
  :vc (copilot
       :url "https://github.com/copilot-emacs/copilot.el")
  :defer t
  :init
  (setq copilot-node-executable "/opt/homebrew/bin/node")
  (setq copilot-idle-delay 0.05)
  (setq copilot-max-char (* 500 1000))  ; Default is 100,000

  ;; Toggling `copilot-mode'
  (defun sthenno/copilot-on ()
    (interactive)
    (copilot-mode 1))

  (defun sthenno/copilot-off ()
    (interactive)
    (copilot-mode -1))

  :config
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-major-mode-alist  '("python-ts" . "python"))

  ;; Hooks
  (add-hook 'python-mode-hook #'sthenno/copilot-on)

  :bind ((:map prog-mode-map
               ("C-x c" . sthenno/turn-on-copilot)
               ("C-x C" . sthenno/turn-off-copilot))
         (:map copilot-completion-map
               ("<tab>"   . copilot-accept-completion)
               ("<right>" . copilot-accept-completion-by-line)
               ("<left>"  . copilot-clear-overlay)
               ("RET"     . copilot-clear-overlay))))

(provide 'init-eglot)
