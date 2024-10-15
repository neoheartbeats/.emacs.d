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

(setq treesit-font-lock-level 4)

;;; Terminal integration
(use-package vterm
  :ensure t
  :config
  (setq vterm-always-compile-module t))

(use-package vterm-toggle
  :ensure t
  :after (vterm)
  :config

  ;; Show vterm buffer in bottom side
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  
  ;; Key bindings
  (keymap-global-set "s-t" #'vterm-toggle)
  
  ;; Switch to next vterm buffer
  (keymap-set vterm-mode-map "s-<right>" #'vterm-toggle-forward)

  ;; Switch to previous vterm buffer
  (keymap-set vterm-mode-map "s-<left>" #'vterm-toggle-backward))

;;; Initialize `eglot'
(use-package eglot
  :ensure t
  :config

  ;; Hooks
  (add-hook 'python-base-mode-hook #'eglot-ensure)
  (setq eglot-server-programs '((python-mode . ("pyright-langserver" "--stdio"))))
  
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;; Automatically confirm .dir-locals.el files
(setq-default enable-local-variables :safe)

;;; Python project management using Conda
(use-package conda
  :ensure t
  :init (conda-env-initialize-interactive-shells)
  :config
  (setopt conda-anaconda-home "~/cond/")
  
  ;; Enable auto-activation
  (conda-env-autoactivate-mode 1)

  ;; Automatically activate a Conda environment on the opening of a file
  (add-hook 'find-file-hook (lambda ()
                              (when (bound-and-true-p conda-project-env-path)
                                (conda-env-activate-for-buffer))))

  ;; Displaying the currently active environment on the `mode-line'
  (add-hook 'python-base-mode-hook #'conda-mode-line-setup))

(setopt python-indent-offset 4)

;; Reformat Python buffers using the Black formatter
(use-package blacken
  :ensure t
  :config

  ;; Hooks
  (add-hook 'python-base-mode-hook #'(lambda ()
                                       (blacken-mode 1)))

  ;; Formatting buffers
  (defun sthenno/python-format-buffer ()
    (interactive)

    ;; pip install isort
    (python-sort-imports)
    (blacken-buffer))
  :bind (:map python-base-mode-map
              ("s-i" . blacken-buffer)))

;; JSON files
;; (add-hook 'json-ts-mode-hook #'(lambda ()
;;                                  (so-long-mode 1)))

;;; gptel: A simple LLM client for Emacs

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'sthenno
        gptel-backend (gptel-make-openai "vLLM"
                        :host "192.168.100.128:8000"
                        :protocol "http"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :key "EMPTY"
                        :models '(sthenno)))
  ;; System messages
  (setq gptel-directives
        '((default   . "You are a helpful assistant living in Emacs.")
          (translate . "Please translate the input from English to Simplified Chinese.")
          (dict      . "Please describe the vocabulary in Chinese.")))
  
  ;; LLM request options
  (setq gptel-max-tokens 1024
        gptel-temperature 0.70)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Chat UI options
  (setq gptel-default-mode #'org-mode)
  
  ;; Scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  
  ;; Move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Org-mode UI options
  (setq gptel-org-branching-context t)

  ;; Custom functions
  (defun sthenno/gptel-buffer ()
    "Open the `gptel' buffer."
    (interactive)
    (let ((buff "*LLM*"))
      (gptel buff)
      (pop-to-buffer buff)))

  ;; Key bindings
  (keymap-global-set "s-l" #'sthenno/gptel-buffer)

  (keymap-set gptel-mode-map "s-<return>" #'gptel-send))

;;; GitHub Copilot
;;
;; After installing this, run `copilot-login'
(use-package copilot
  :vc (copilot
       :url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :init
  (setq copilot-node-executable "/opt/homebrew/bin/node")
  
  ;; Toggling `copilot-mode'
  (defun sthenno/copilot-on ()
    (interactive)
    (copilot-mode 1))

  :config
  
  ;; Hooks
  (add-hook 'python-base-mode-hook #'sthenno/copilot-on)

  :bind ((:map prog-mode-map
               ("C-x c" . sthenno/copilot-on)
               ("C-x C" . sthenno/copilot-off))
         (:map copilot-completion-map
               ("<tab>"   . copilot-accept-completion)
               ("TAB"     . copilot-accept-completion)
               ("<right>" . copilot-accept-completion-by-line)
               ("<left>"  . copilot-clear-overlay)
               ("RET"     . copilot-clear-overlay))))

(provide 'init-eglot)
