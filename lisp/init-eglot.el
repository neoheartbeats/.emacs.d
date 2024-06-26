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
;; (use-package treesit-auto
;;   :straight t
;;   :config (global-treesit-auto-mode 1))

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
  (add-hook 'python-ts-mode #'eglot-ensure)

  ;; Config `corfu' for `eglot', see also `init-comp'
  ;; Continuously update the candidates
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :bind (:map eglot-mode-map
	          ("<f6>" . eglot-rename)))

;; Note `project' is a dependency for `eglot' but not declared by `projectile'
;; See https://github.com/radian-software/straight.el/issues/1146 [TODO]

;; Speed up
;; (use-package eglot-booster
;;   :straight (eglot-booster
;;              :type git
;;              :host github
;;              :repo "jdtsmith/eglot-booster")
;;   :init
;;   (add-to-list 'exec-path (expand-file-name "bin/" user-emacs-directory))
;;   :config
;;   (setq eglot-booster-no-remote-boost t)
;;   (eglot-booster-mode 1))

;; Auto confirm `.dir-locals.el' files
(setq-default enable-local-variables :safe)


;;; Python project management [TODO]
(use-package python
  :straight t
  :config
  (setq python-indent-offset 4))

;; Reformat python buffers using the `black' formatter
(use-package blacken
  :straight t
  :config (add-hook 'python-ts-mode-hook #'(lambda ()
					                         (blacken-mode 1)))
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


;;; Python API: sthenno-endpoints Client
;;
;; Translation
(require 'url)
(require 'json)

(setq sthenno-endpoint-u "http://localhost:8000")

(defun sthenno-post (endpoint content)
  "Send a json formatted post request to the specified ENDPOINT using CONTENT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
			                (json-encode `(("content" . ,content))) 'utf-8))
         (url (format "%s/%s/" sthenno-endpoint-u endpoint))
         response)
    (with-current-buffer (url-retrieve-synchronously url t) ; t means silent
      (goto-char url-http-end-of-headers)
      (setq response (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    (json-read-from-string (decode-coding-string response 'utf-8))))

(defun sthenno-trans-to-zh (content)
  "Translate the CONTENT to Simplified Chinese.
Return the translation."
  (let* ((obj (sthenno-post "trans_to_zh" content))
	     (translation (cdr (assoc 'translation obj))))
    translation))

(defun sthenno-trans-to-en (content)
  "Translate the CONTENT to English.
Return the translation."
  (let* ((obj (sthenno-post "trans_to_en" content))
	     (translation (cdr (assoc 'translation obj))))
    translation))

(defun sthenno-trans-to-zh-target (target)
  (let ((translation (sthenno-trans-to-zh target)))
    (kill-new translation)
    (message (format "Translation: %s" translation))))

(defun sthenno-trans-to-en-target (target)
  (let ((translation (sthenno-trans-to-en target)))
    (kill-new translation)
    (message (format "Translation: %s" translation))))

;; Binding keys (see also `init-comp') [TODO]
;; (use-package embark
;;   :config
;;   (defun embark-target-word-at-point ()
;;     "Target the word at point for Embark."
;;     (save-excursion
;;       (let ((word (thing-at-point 'word)))
;;         (save-match-data
;;           (when word
;;             (cons 'word word))))))
;;   (add-to-list 'embark-target-finders 'embark-target-word-at-point)

;;   ;; (keymap-set embark-general-map "t z" #'sthenno-trans-to-zh-target)
;;   ;; (keymap-set embark-general-map "t e" #'sthenno-trans-to-en-target)
;;   (bind-keys :map embark-general-map
;;              ("T" . sthenno-trans-to-zh-target)
;;              ("E" . sthenno-trans-to-en-target)))

(provide 'init-eglot)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
















