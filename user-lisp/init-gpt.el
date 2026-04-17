;;; init-gpt.el --- AI assistance -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the interactive AI assistant configuration.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(use-package gptel
  :ensure t
  :config
  (setopt gptel-default-mode #'org-mode
          gptel-org-branching-context t)
  (setopt gptel-model 'sthenno
          gptel-backend (gptel-make-openai "local"
                          :protocol "http"
                          :host "192.168.100.204:8000"
                          :endpoint "/v1/chat/completions"
                          :stream t
                          :key "tmp"
                          :models '(sthenno)))
  :bind ((:map global-map
               ("s-p" . gptel)
               ("s-<return>" . gptel-send))
         (:map gptel-mode-map
               ("s-<return>" . gptel-send))))

(provide 'init-gpt)
