;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file complement the develop environment for specific languages.
;;

;;; Code:

;;; Setup `treesit' for better performance for processing coding syntax
;;
;; Command `treesit-auto-install-all' is required if the tree-sitter grammar
;; libs have not been configured already
(use-package treesit-auto
  :straight t
  :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
(setq treesit-font-lock-level 4)


;; Initialize `eglot'
(use-package eglot
  :config

  ;; Use Pyright as the default language server
  (add-to-list 'eglot-server-programs
    '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-hook 'python-ts-mode #'eglot-ensure))

;; Auto confirm `.dir-locals.el' files
(setq-default enable-local-variables :safe)


;;; Python project management
;;
;; Environment management using conda
;; (use-package conda
;;   :straight t
;;   :config
;;   (setq conda-env-home-directory (expand-file-name "~/anaconda3/"))
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-autoactivate-mode 1)
;;   (add-hook 'find-file-hook #'(lambda ()
;; 				                        (when (bound-and-true-p conda-project-env-path)
;;                                   (conda-env-activate-for-buffer)))))

;; (setq
;;   python-indent-guess-indent-offset t
;;   python-indent-guess-indent-offset-verbose nil)

;; Reformat python buffers using the `black' formatter
(use-package blacken
  :straight t
  :config (add-hook 'python-ts-mode-hook #'blacken-mode)
  :bind
  (:map python-ts-mode-map
    ("s-i" . blacken-buffer)))


(use-package indent-bars
  :straight (indent-bars
              :type git
              :host github
              :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  :config
  (setq
    indent-bars-color '(highlight :face-bg t :blend 0.2)
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
    indent-bars-highlight-current-depth '(:blend 0.8)
    indent-bars-depth-update-delay 0.0
    indent-bars-starting-column 0
    indent-bars-zigzag nil
    indent-bars-display-on-blank-lines t)

  (add-hook 'org-mode-hook #'(lambda ()
                               (setq-locals indent-bars-spacing-override 2)))
  :hook ((python-ts-mode org-mode) . indent-bars-mode))


;;; AI Integration
;;
;; GitHub Copilot
(use-package copilot
  :straight (
	            :host github
	            :repo "copilot-emacs/copilot.el"
	            :files ("*.el"))
  :config
  (define-key global-map (kbd "s-.") #'copilot-accept-completion))


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
