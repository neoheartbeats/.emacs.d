;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

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
  :config
  (setopt treesit-auto-install t)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

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
  (setq eglot-server-programs
        `(((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

  ;; Hooks
  (add-hook 'python-ts-mode-hook 'eglot-ensure)

  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;; Boost eglot using lsp-booster

(use-package eglot-booster
  :vc (eglot-booster
       :url "https://github.com/jdtsmith/eglot-booster"
       :branch "main")
  :ensure t
  :after eglot
  :init (add-to-list 'exec-path (locate-user-emacs-file "bin/"))
  :config (eglot-booster-mode 1))

;;; Python

(use-package python
  :demand t
  :init
  (require 'python)
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil
                python-indent-guess-indent-offset-verbose nil)

  :config

  ;; Python project management
  (defun sthenno/python-venv ()
    "Activate Python environment managed by uv based on current
project directory.
Looks for ‘.venv’ directory in project root and activates the Python interpreter."
    (interactive)
    (let* ((pr (project-root (project-current t)))
           (venv-path (expand-file-name ".venv" pr))
           (python-path (expand-file-name "bin/python" venv-path)))
      (if (file-exists-p python-path)
          (progn
            ;; Set Python interpreter path
            (setq python-shell-interpreter python-path)

            ;; Update `exec-path' to include the venv’s bin directory
            (let ((venv-bin-dir (file-name-directory python-path)))
              (setq exec-path (cons venv-bin-dir
                                    (remove venv-bin-dir exec-path))))

            ;; Update PATH environment
            (setenv "PATH" (concat (file-name-directory python-path)
                                   path-separator
                                   (getenv "PATH")))
            (setenv "VIRTUAL_ENV" venv-path)

            ;; Remove PYTHONHOME if it exists
            (setenv "PYTHONHOME" nil)

            (message "Activated UV Python environment at %s" venv-path))
        (message "No UV Python environment found in %s" pr))))

  (add-hook 'python-ts-mode-hook 'sthenno/python-venv)

  :bind ((:map python-ts-mode-map
               ;;  ([remap forward-paragraph] . python-nav-forward-statement)
               ;; ([remap backward-paragraph] . python-nav-backward-statement)
               ;; ([remap move-beginning-of-line] . python-nav-beginning-of-statement)
               ;; ([remap move-end-of-line] . python-nav-end-of-statement)
               ("s-<up>" . python-nav-beginning-of-block)
               ("s-<down>" . python-nav-end-of-block)
               ("C-x m" . python-nav-if-name-main)
               ;; ("<tab>" . python-indent-shift-right)
               ;; ("S-<tab>" . python-indent-shift-left)
               )))

;;; Flymake

;; (use-package flymake
;;   :config
;;   ;; (setq flymake-no-changes-timeout nil)
;;   (setq flymake-start-on-save-buffer t)
;;   (setq flymake-mode-line-lighter "FM")
;;   (setq flymake-indicator-type 'margins)
;;   :hook (prog-mode . flymake-mode)
;;   :bind ((:map flymake-mode-map
;;                ("s-<down>" . flymake-goto-next-error)
;;                ("s-<up>"   . flymake-goto-prev-error))))

(use-package flymake-ruff
  :ensure t
  :config (add-hook 'python-ts-mode-hook 'flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :config (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))

;;; AI

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode #'org-mode)

  ;; System messages
  (setq gptel-directives
        '((default . "You are a helpful assistant living in Emacs. Respond concisely.")))

  ;; Generation options
  (setq gptel-max-tokens 1024
        gptel-temperature 0.70)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Use OpenAI as the default backend
  ;;
  (setq gptel-model 'claude_opus4
        gptel-backend (gptel-make-openai "idealab"
                        :protocol "https"
                        :host "idealab.alibaba-inc.com/api/openai"
                        :stream t
                        :key (gptel-api-key-from-auth-source "idealab.alibaba-inc.com")
                        :models '(Qwen3-32B
                                  claude37_sonnet
                                  claude_opus4
                                  claude_sonnet4
                                  o3-0416-global
                                  o4-mini-0416-global
                                  gemini-2.5-pro-preview-05-06
                                  gemini-2.5-flash-preview-05-20)))

  ;; UI
  ;;
  ;; Scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; Move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Org mode specific options
  ;;
  ;; Use the lineage of the current heading as the context for gptel in Org buffers.
  (setq gptel-org-branching-context t)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=In[*]≔= ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "=Out[*]≔= ")

  ;; Functions of the `gptel' buffer
  (defun sthenno/gptel-to-buffer ()
    "Open the gptel buffer."
    (interactive)
    (let ((buff (concat "*" (symbol-name gptel-model) "*")))
      (gptel buff)
      (turn-on-visual-line-mode)
      (diminish 'visual-line-mode)
      (switch-to-buffer buff)))

  :bind ((:map global-map
               ("s-p" . sthenno/gptel-to-buffer)
               ("s-<return>" . gptel-send))
         (:map gptel-mode-map
               ("s-<return>" . gptel-send))))

;;; Teminal support

;; (use-package vterm :ensure t)

;;; GitHub Copilot

;; (use-package copilot
;;   :vc (copilot
;;        :url "https://github.com/copilot-emacs/copilot.el"
;;        :branch "main")
;;   :ensure t
;;   :init (setq copilot-node-executable "/opt/homebrew/bin/node")
;;   :bind ((:map prog-mode-map
;;                ("C-x c" . copilot-mode))
;;          (:map copilot-completion-map
;;                ("TAB"      . copilot-accept-completion)
;;                ("<tab>"    . copilot-accept-completion)
;;                ("<right>" . copilot-accept-completion)
;;                ("<escape>" . copilot-clear-overlay))))

(provide 'init-eglot)

;;; init-eglot.el ends here
