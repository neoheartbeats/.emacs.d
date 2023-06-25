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
  :defer t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize `eglot'
(use-package eglot
  :ensure t
  :defer t
  :config
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  :bind
  ((:map python-ts-mode-map
         ("s-i" . eglot-format-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;
;; Python environment management
;;
;;
;; Configure `conda' environment
(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home "~/anaconda3/")
  (setq conda-env-home-directory (expand-file-name "~/anaconda3/"))
  (setq conda-env-autoactivate-mode t)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(setq python-shell-interpreter (expand-file-name
                                "envs/Navras/bin/python" conda-anaconda-home)
      python-shell-interpreter-args "-i"
      python-shell--interpreter python-shell-interpreter
      python-shell--interpreter-args python-shell-interpreter-args
      python-shell-prompt-detect-failure-warning nil
      python-shell-completion-native-enable nil)

;; Code navigation, documentation lookup and completion for Python
(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish (anaconda-mode anaconda-eldoc-mode)
  :hook (python-ts-mode . anaconda-mode)
  :bind
  (:map anaconda-mode-map
        ("M-?" . anaconda-mode-show-doc)
        ("M-." . anaconda-mode-find-definitions)
        ("M-," . anaconda-mode-find-assignments)
        ("M-r" . anaconda-mode-find-references)
        ("M-*" . anaconda-mode-go-back)))


(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
