;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup `treesit'
(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configure and install language modules
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Remap modes to use `treesit'
(setq major-mode-remap-alist '((python-mode . python-ts-mode)))

;; Initialize `eglot'
(use-package eglot
  :ensure t
  :config
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  :bind
  ((:map python-ts-mode-map
         ("s-i" . eglot-format-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;
;; Configure `conda' environment
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base/")
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; Code navigation, documentation lookup and completion for Python
(use-package anaconda-mode
  :ensure t
  :hook (python-ts-mode . anaconda-mode)
  :bind
  (:map anaconda-mode-map
        ("M-?" . anaconda-mode-show-doc)
        ("M-." . anaconda-mode-find-definitions)
        ("M-," . anaconda-mode-find-assignments)
        ("M-r" . anaconda-mode-find-references)
        ("M-*" . anaconda-mode-go-back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs IPython Notebook
(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete t)
  (setq ein:use-smartrep t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indentation guide
(use-package highlight-indent-guides
  :ensure t
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "")
  (setq highlight-indent-guides-method 'character))

(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
