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

;; Speed up
(use-package eglot-booster
  :straight (eglot-booster
             :type git
             :host github
             :repo "jdtsmith/eglot-booster")
  :init
  (add-to-list 'exec-path (expand-file-name "bin/" user-emacs-directory))
  :config
  (setq eglot-booster-no-remote-boost t)
  (eglot-booster-mode 1))

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
   indent-bars-color '(highlight :face-bg t :blend 0.4)
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.8)
   indent-bars-starting-column 0
   indent-bars-zigzag nil
   indent-bars-display-on-blank-lines t)

  (defun indent-bars--guess-spacing ()
    "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
    (cond
     ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
      py-indent-offset)
     ((and (derived-mode-p 'python-mode) (boundp 'python-indent-offset))
      python-indent-offset)
     ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
      ruby-indent-level)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
      scala-indent:step)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
      scala-mode-indent:step)
     ((and (or (derived-mode-p 'scss-mode) (derived-mode-p 'css-mode))
	   (boundp 'css-indent-offset))
      css-indent-offset)
     ((and (derived-mode-p 'nxml-mode) (boundp 'nxml-child-indent))
      nxml-child-indent)
     ((and (derived-mode-p 'coffee-mode) (boundp 'coffee-tab-width))
      coffee-tab-width)
     ((and (derived-mode-p 'js-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (derived-mode-p 'js2-mode) (boundp 'js2-basic-offset))
      js2-basic-offset)
     ((and (derived-mode-p 'sws-mode) (boundp 'sws-tab-width))
      sws-tab-width)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
      web-mode-markup-indent-offset)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
      web-mode-html-offset)
     ((and (local-variable-p 'c-basic-offset) (numberp c-basic-offset))
      c-basic-offset)
     ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
      yaml-indent-offset)
     ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
      elixir-smie-indent-basic)
     ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
      lisp-body-indent)
     ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
      cobol-tab-width)
     ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
      tab-width)
     ((derived-mode-p 'nix-mode)
      tab-width)
     ((and (derived-mode-p 'nix-ts-mode) (boundp 'nix-ts-mode-indent-offset))
      nix-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-ts-mode) (boundp 'json-ts-mode-indent-offset))
      json-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (boundp 'standard-indent) standard-indent))
     ((and (derived-mode-p 'org-mode) (boundp 'org-list-indent-offset))
      org-list-indent-offset)
     (t 4)))
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
