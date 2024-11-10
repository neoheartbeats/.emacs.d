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
;; Command `treesit-auto-install-all' is required if the tree-sitter grammar libs have
;; not been configured already
(use-package treesit-auto
  :ensure t
  :demand t
  :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(setq major-mode-remap-alist '((sh-mode         . bash-ts-mode)
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

;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
;; (setq treesit-font-lock-level 4)

;;; Terminal integration

;; (use-package vterm
;;   :ensure t
;;   :config (setq vterm-always-compile-module t))

;; (use-package vterm-toggle
;;   :ensure t
;;   :after (vterm)
;;   :config

;;   ;; Show vterm buffer in bottom side
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (add-to-list 'display-buffer-alist
;;                '((lambda (buffer-or-name _)
;;                    (let ((buffer (get-buffer buffer-or-name)))
;;                      (with-current-buffer buffer
;;                        (or (equal major-mode 'vterm-mode)
;;                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;;                  (display-buffer-reuse-window display-buffer-at-bottom)
;;                  (display-buffer-reuse-window display-buffer-in-direction)
;;                  (direction . bottom)
;;                  (dedicated . t)
;;                  (reusable-frames . visible)
;;                  (window-height . 0.4)))

;;   ;; Key bindings
;;   (keymap-global-set "s-t" #'vterm-toggle)

;;   ;; Switch to next vterm buffer
;;   (keymap-set vterm-mode-map "s-<right>" #'vterm-toggle-forward)

;;   ;; Switch to previous vterm buffer
;;   (keymap-set vterm-mode-map "s-<left>" #'vterm-toggle-backward))

;;; Initialize `eglot'
(use-package eglot
  :ensure t
  :config
  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))

  ;; Hooks
  (add-hook 'prog-mode-hook #'eglot-ensure)
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;; Automatically confirm .dir-locals.el files
;; (setq-default enable-local-variables :safe)

;;; Python project management using Conda
;; (use-package conda
;;   :ensure t
;;   :init (conda-env-initialize-interactive-shells)
;;   :config
;;   (setopt conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base/")

;;   ;; Enable auto-activation
;;   (conda-env-autoactivate-mode 1)

;;   ;; Automatically activate a Conda environment on the opening of a file
;;   (add-hook 'find-file-hook (lambda ()
;;                               (when (bound-and-true-p conda-project-env-path)
;;                                 (conda-env-activate-for-buffer))))

;;   ;; Displaying the currently active environment on the `mode-line'
;;   (add-hook 'python-mode-hook #'conda-mode-line-setup))

;; (setq python-indent-offset 4)
;; (setq python-indent-guess-indent-offset t)
;; (setq python-indent-guess-indent-offset-verbose nil)

;; Reformat Python buffers using the Black formatter
;; (use-package blacken
;;   :ensure t
;;   :config

;;   ;; Hooks
;;   (add-hook 'python-mode-hook #'blacken-mode)

;;   ;; Formatting buffers
;;   (defun sthenno/python-format-buffer ()
;;     (interactive)

;;     ;; pip install isort
;;     (python-sort-imports)
;;     (blacken-buffer))

;;   :bind (:map python-base-mode-map
;;               ("s-i" . sthenno/python-format-buffer)))

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
                        :key "sk-1234"
                        :models '(sthenno)))
  ;; System messages
  (setq gptel-directives '((default . "You are Sthenno.")))

  ;; LLM request options
  (setq gptel-max-tokens 1024
        gptel-temperature 0.70)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Chat UI options
  (setq gptel-default-mode 'org-mode)

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
    (let ((buff "*vLLM*"))
      (gptel buff)
      (pop-to-buffer buff)))

  ;; Key bindings
  (keymap-global-set "s-l" #'sthenno/gptel-buffer)

  (keymap-set gptel-mode-map "S-<return>" #'gptel-send)
  (keymap-set org-mode-map "S-<return>" #'gptel-send))

;;; GitHub Copilot
;;
;; After installing this, run `copilot-login'

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

;;   ;; Hooks
;;   (add-hook 'python-mode-hook #'sthenno/copilot-on)

;;   :bind ((:map prog-mode-map
;;                ("C-x c" . sthenno/copilot-on)
;;                ("C-x C" . sthenno/copilot-off))
;;          (:map copilot-completion-map
;;                ("<tab>"   . copilot-accept-completion)
;;                ("TAB"     . copilot-accept-completion)
;;                ("<right>" . copilot-accept-completion-by-line)
;;                ("<left>"  . copilot-clear-overlay)
;;                ("RET"     . copilot-clear-overlay))))

;;; codeium.el

;; (use-package codeium
;;   :vc (codeium
;;        :url "https://github.com/Exafunction/codeium.el"
;;        :branch "main")
;;   :init
;;   (add-hook 'prog-mode-hook #'(lambda ()
;;                                 (add-to-list 'completion-at-point-functions
;;                                              #'codeium-completion-at-point)))

;;   ;; codeium-completion-at-point is autoloaded, but you can optionally set a timer,
;;   ;; which might speed up things as the codeium local language server takes ~0.2s to
;;   ;; start up
;;   (add-hook 'emacs-startup-hook #'(lambda ()
;;                                     (run-with-timer 0.1 nil #'codeium-init)))
;;   :config

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable #'(lambda (api)
;;                                      (not (memq api '(CancelRequest
;;                                                       Heartbeat
;;                                                       AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local
;;   ;; language server
;;   (setq codeium-api-enabled #'(lambda (api)
;;                                 (memq api '(GetCompletions
;;                                             Heartbeat
;;                                             CancelRequest
;;                                             GetAuthToken
;;                                             RegisterUser
;;                                             auth-redirect
;;                                             AcceptCompletion))))

;;   ;; Limiting the string sent to codeium for better performance
;;   (defun sthenno/codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min))
;;                                     (min (+ (point) 1000) (point-max))))

;;   ;; if you change the text, you should also change the cursor_offset
;;   (defun sthenno/codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000)
;;                                           (point-min))
;;                                      (point))))
;;   (setq codeium/document/text #'sthenno/codeium/document/text)
;;   (setq codeium/document/cursor_offset #'sthenno/codeium/document/cursor_offset))

(provide 'init-eglot)
