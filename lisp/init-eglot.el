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

(use-package treesit-auto
  :ensure t
  :demand t
  :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(setq-default major-mode-remap-alist '((sh-mode         . bash-ts-mode)
                                       (js-mode         . js-ts-mode)
                                       (json-mode       . json-ts-mode)
                                       (python-mode     . python-ts-mode)
                                       (typescript-mode . typescript-ts-mode)
                                       (yaml-mode       . yaml-ts-mode)))

;; Append *-mode-hook to *-ts-mode-hook for modes in `major-mode-remap-list'
(mapc #'(lambda (major-mode-remap)
          (let ((major-mode-hook
                 (intern (concat (symbol-name (car major-mode-remap)) "-hook")))
                (major-ts-mode-hook
                 (intern (concat (symbol-name (cdr major-mode-remap)) "-hook"))))
            (add-hook major-ts-mode-hook `(lambda ()
                                            (run-hooks (quote ,major-mode-hook))))))
      major-mode-remap-alist)

(setq-default treesit-font-lock-level 4)

;;; Initialize `eglot'

(use-package eglot
  :ensure t
  :demand t
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ruff" "server")))

  ;; Hooks
  (add-hook 'python-mode-hook 'eglot-ensure)

  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;;; Python project management using Conda

(use-package conda
  :ensure t
  :init (conda-env-initialize-interactive-shells)
  :config
  (setopt conda-anaconda-home "/opt/homebrew/Caskroom/miniforge/base/")

  ;; Enable auto-activation
  (conda-env-autoactivate-mode 1)

  ;; Displaying the currently active environment on the `mode-line'
  (add-hook 'python-mode-hook 'conda-mode-line-setup)

  :bind ((:map python-mode-map
               ("C-c a" . conda-env-activate))))

(setq-default python-indent-offset 4)
;; (setq python-indent-guess-indent-offset nil)
;; (setq python-indent-guess-indent-offset-verbose nil)

;;; GitHub Copilot

(use-package copilot
  :vc (copilot
       :url "https://github.com/copilot-emacs/copilot.el"
       :branch "main")
  :init (setq copilot-node-executable "/opt/homebrew/bin/node")
  :config

  ;; Toggling `copilot-mode'
  (defun sthenno/copilot-on ()
    (interactive)
    (copilot-mode 1))

  (defun sthenno/copilot-off ()
    (interactive)
    (copilot-mode -1))

  ;; Hooks
  (add-hook 'python-mode-hook #'sthenno/copilot-on)

  :bind ((:map prog-mode-map
               ("C-x c" . sthenno/copilot-on)
               ("C-x C" . sthenno/copilot-off))
         (:map copilot-completion-map
               ;; ("<tab>"   . copilot-accept-completion)
               ;; ("TAB"     . copilot-accept-completion)
               ;; ("<right>" . copilot-accept-completion-by-line)
               ("<right>"  . copilot-accept-completion)
               ("<return>" . copilot-accept-completion)
               ("<escape>" . copilot-clear-overlay))))

;;; _
(provide 'init-eglot)
