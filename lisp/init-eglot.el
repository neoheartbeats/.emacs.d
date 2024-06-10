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


;;; sthenno-endpoints
;;
;; Translation
(require 'url)
(require 'url-http)
(require 'json)

(setq sthenno-endpoint-u "http://localhost:8000")

(defun sthenno-post (endpoint content)
  "Send a JSON formatted POST request to the specified endpoint using
the given content."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode `(("content" . ,content))))
         (url (format "%s/%s" sthenno-endpoint-u endpoint))
         response)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (setq response (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    (json-read-from-string (decode-coding-string response 'utf-8))))

(defun sthenno-trans-to-zh (content)
  (let* ((obj (sthenno-post "trans_to_zh" content))
	 (translation (cdr (assoc 'translation obj))))
    translation))

(defun sthenno-selected-text ()
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
	     (end (region-end))
	     (text (buffer-substring-no-properties start end)))
	text)
    (message "No text selected.")))

(defun sthenno-trans-to-zh-selected ()
  (interactive)
  (let* ((text (sthenno-selected-text))
	 (translation (sthenno-trans-to-zh text)))
    (kill-new translation)
    (message (format "Translation: %s" translation))
    translation))

(bind-keys* :map org-mode-map
	    ("C-c t" . sthenno-trans-to-zh-selected))

(provide 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
