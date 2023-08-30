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
  :config
  (global-treesit-auto-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize `eglot'
(use-package eglot
  :ensure t
  :defer t
  :config
  (add-hook 'python-ts-mode-hook #'eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;
;; Python environment management
;;
;; Configure `conda' environment
(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home "~/anaconda3/")
  (setq conda-env-home-directory "~/anaconda3/envs/")
  (setq conda-env-autoactivate-mode t)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; TODO
(setq python-shell-interpreter "~/anaconda3/envs/navras/bin/python"
      python-shell-interpreter-args "-i"
      python-shell--interpreter python-shell-interpreter
      python-shell--interpreter-args python-shell-interpreter-args
      python-shell-prompt-detect-failure-warning nil
      python-shell-completion-native-enable nil)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

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

;; Reformat python buffers using the `black' formatter
(use-package blacken
  :ensure t
  :defer t
  :config

  ;; Auto reformat the buffer after saving
  (add-hook 'python-ts-mode-hook #'(lambda ()
                                     (blacken-mode 1)))
  :bind
  (:map python-ts-mode-map
        ("s-i" . blacken-buffer)))

(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
