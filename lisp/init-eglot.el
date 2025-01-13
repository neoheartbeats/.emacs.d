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
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ruff" "server")))

  ;; Hooks
  (add-hook 'python-mode-hook 'eglot-ensure)

  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;;; [FIXME: This should be replaced by "uv"] Python project management using Conda

;; (use-package conda
;;   :ensure t
;;   :init (conda-env-initialize-interactive-shells)
;;   :config
;;   (setopt conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base/")

;;   ;; Enable auto-activation
;;   (conda-env-autoactivate-mode 1)

;;   ;; Displaying the currently active environment on the `mode-line'
;;   (add-hook 'python-mode-hook 'conda-mode-line-setup)

;;   :bind ((:map python-mode-map
;;                ("C-c a" . conda-env-activate))))

(setq-default python-indent-offset 4)
(setq-default python-indent-guess-indent-offset nil)
(setq-default python-indent-guess-indent-offset-verbose nil)

;;; Teminal support

(use-package vterm
  :ensure t)

;;; gptel: A simple LLM client for Emacs

;; NOTE: Since support "mcp" in Emacs is highly experimental, LLM related functions are
;; temporarily disabled.

;; (use-package gptel
;;   :load-path "~/.emacs.d/site-lisp/gptel-tools/"
;;   :config

;;   ;; LLM request options
;;   (setq gptel-api-key "sk-tmp")         ; Enough for LiteLLM
;;   (setq gptel-backend
;;         (gptel-make-openai "sthenno"    ; Name
;;           :stream t                     ; Stream responses
;;           :protocol "http"              ; Use http for local requests
;;           :host "192.168.100.127:8000"  ; Server location
;;           :models '(sthenno)))
;;   (setq gptel-model 'sthenno)
;;   (setq gptel-stream t)
;;   (setq gptel-max-tokens 8192)
;;   (setq gptel-temperature 0.70)

;;   ;; Chat UI options
;;   (setq gptel-default-mode 'org-mode)

;;   ;; Scroll automatically as the response is inserted
;;   (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

;;   ;; Let the cursor to move to the next prompt after the response is inserted
;;   (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

;;   ;; Agentic system
;;   (setq gptel-use-tools t)

;;   (defun sthenno/mcp-init ()
;;     (interactive)
;;     (require 'mpc)
;;     (mcp-connect-server
;;      "filesystem" "npx" '("-y" "@modelcontextprotocol/server-filesystem"
;;                           "/Users/sthenno/.emacs.d/")
;;      :initial-callback
;;      #'(lambda (connection)
;;          (message "%s connection" (jsonrpc-name connection)))
;;      :tools-callback
;;      #'(lambda (connection tools)
;;          (message "%s tools: %s" (jsonrpc-name connection) tools))
;;      :prompts-callback
;;      #'(lambda (connection prompts)
;;          (message "%s prompts: %s" (jsonrpc-name connection) prompts))
;;      :resources-callback
;;      #'(lambda (connection resources)
;;          (message "%s resources: %s" (jsonrpc-name connection) resources)))

;;     (gptel-make-tool
;;      :function (lambda (path filename content)
;;                  (let ((full-path (expand-file-name filename path)))
;;                    (with-temp-buffer
;;                      (insert content)
;;                      (write-file full-path))
;;                    (format "Created file %s in %s" filename path)))
;;      :name "create_file"
;;      :description "Create a new file with the specified content"
;;      :args (list '(:name "path"
;;                          :type "string"
;;                          :description "The directory where to create the file")
;;                  '(:name "filename"
;;                          :type "string"
;;                          :description "The name of the file to create")
;;                  '(:name "content"
;;                          :type "string"
;;                          :description "The content to write to the file"))
;;      :category "filesystem")
;;     )

;;   (sthenno/mcp-init)

;;   :bind ((:map global-map
;;                ("s-p" . gptel))
;;          (:map org-mode-map
;;                ("s-<return>" . gptel-send))))

;;; Model Context Protocol Emacs sdk

;; (require 'mcp)

;;; GitHub Copilot

;; (use-package copilot
;;   :vc (copilot
;;        :url "https://github.com/copilot-emacs/copilot.el"
;;        :branch "main")
;;   :init (setq copilot-node-executable "/opt/homebrew/bin/node")
;;   :config

;;   ;; Toggling `copilot-mode'
;;   (defun sthenno/copilot-on ()
;;     (interactive)
;;     (copilot-mode 1))

;;   (defun sthenno/copilot-off ()
;;     (interactive)
;;     (copilot-mode -1))

;;   ;; Hooks
;;   (add-hook 'python-mode-hook #'sthenno/copilot-on)

;;   :bind ((:map prog-mode-map
;;                ("C-x c" . sthenno/copilot-on)
;;                ("C-x C" . sthenno/copilot-off))
;;          (:map copilot-completion-map
;;                ("<right>"  . copilot-accept-completion-by-line)
;;                ("<return>" . copilot-accept-completion)
;;                ("<escape>" . copilot-clear-overlay))))

;;; _
(provide 'init-eglot)
