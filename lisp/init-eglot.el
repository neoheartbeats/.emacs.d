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
;; (use-package treesit-auto
;;   :ensure t
;;   :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
;;
(setq treesit-font-lock-level 4)


;; Initialize `eglot'
;;
(use-package eglot
  :after (project)
  :init
  
  ;; Config `corfu' for `eglot', see also `init-comp'
  ;; Continuously update the candidates
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  :config

  ;; Python
  ;; Use Pyright as the default language server
  ;;
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))

  ;; Hooks
  (add-hook 'python-ts-mode #'eglot-ensure)

  :bind (:map eglot-mode-map
              ("<f6>" . eglot-rename)))

;; Speed up
;; (use-package eglot-booster
;;   :straight (eglot-booster
;;              :type git
;;              :host github
;;              :repo "jdtsmith/eglot-booster")
;;   :after (eglot)
;;   :init (add-to-list 'exec-path (expand-file-name "bin/" user-emacs-directory))
;;   :config
;;   (setq eglot-booster-no-remote-boost t)
;;   (eglot-booster-mode 1))

;; Automatically confirm .dir-locals.el files
(setq-default enable-local-variables :safe)


;; Python project management
;;
(setq python-indent-offset 4)

(use-package conda
  :ensure t
  :after (python))

;; Reformat Python buffers using the Black formatter
(use-package blacken
  :ensure t
  :after (python)
  :init (add-hook 'python-ts-mode-hook #'(lambda ()
                                           (blacken-mode 1)))
  :bind (:map python-ts-mode-map
              ("s-p" . blacken-buffer)))


;;; AI Integration
;;
;; LLM client
;;
(use-package gptel
  :ensure t
  :defer t
  :init
  (setq gptel-use-curl t)
  (setq gptel-default-mode #'org-mode)

  ;; System messages
  (setq gptel-directives
        '((default . "You are a helpful assistant living in Emacs.")))

  ;; Generation options
  (setq gptel-max-tokens 512
        gptel-temperature 0.4)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Use Ollama as the default backend
  ;;
  (defun sthenno/gptel-ollama-backend--host (host)
    "Make a function to initialize an Ollama backend using the given HOST."
    `(lambda (model)
       (gptel-make-ollama model
         :host ,host
         :stream t
         :models `(,model))))

  (defun sthenno/gptel-ollama-backend--localhost (model)
    "Initialize an Ollama backend using localhost and the given MODEL."
    (funcall (sthenno/gptel-ollama-backend--host "localhost:11434") model))

  ;; Setup the model
  (let* ((model "phi3")
         (backend (sthenno/gptel-ollama-backend--localhost model)))
    (setq gptel-model model
          gptel-backend backend))

  ;; UI
  ;;
  ;; Scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; Move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  :config
  (setq sthenno/gptel-input-count 0)

  (defun sthenno/gptel-prefix ()
    (setq sthenno/gptel-input-count (1+ sthenno/gptel-input-count))
    (let* ((cnt sthenno/gptel-input-count)
           (prefix-in  (format "_In[%s]:=_ " cnt))
           (prefix-out (format "_Out[%s]=_ " (1- cnt))))
      (setq gptel-prompt-prefix-alist   `((org-mode . ,prefix-in))
            gptel-response-prefix-alist `((org-mode . ,prefix-out)))))

  (add-hook 'gptel-mode-hook         #'sthenno/gptel-prefix)
  (add-hook 'gptel-pre-response-hook #'sthenno/gptel-prefix)

  ;; Functions of the `gptel' buffer
  ;;
  (add-hook 'gptel-mode-hook #'turn-on-visual-line-mode)

  (defun sthenno/gptel-to-buffer ()
    "Open the gptel buffer."
    (interactive)
    (let ((buff "*Φ*"))
      (gptel buff)

      ;; TODO: Add repeat keys to jump back
      (switch-to-buffer buff)))

  (global-set-key (kbd "s-l") #'sthenno/gptel-to-buffer)

  :bind (:map gptel-mode-map
              ("S-<return>" . gptel-send)))


;; GitHub Copilot
;;
;; (use-package copilot
;;   :straight (copilot
;;              :host github
;;              :repo "copilot-emacs/copilot.el"
;;              :files ("*.el"))
;;   :defer t
;;   :init
;;   (setq copilot-node-executable "/opt/homebrew/bin/node")
;;   (setq copilot-idle-delay 0.05)
;;   (setq copilot-max-char (* 500 1000))  ; Default is 100,000

;;   ;; Toggling `copilot-mode'
;;   (defun sthenno/turn-on-copilot ()
;;     (interactive)
;;     (copilot-mode 1))

;;   (defun sthenno/turn-off-copilot ()
;;     (interactive)
;;     (copilot-mode -1))

;;   :config
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   (add-to-list 'copilot-major-mode-alist  '("python-ts" . "python"))

;;   ;; Hooks
;;   (add-hook 'python-ts-mode-hook #'sthenno/turn-on-copilot)

;;   :bind ((:map prog-mode-map
;;                ("C-x c" . sthenno/turn-on-copilot)
;;                ("C-x C" . sthenno/turn-off-copilot))
;;          (:map copilot-completion-map
;;                ("<tab>"   . copilot-accept-completion)
;;                ("<right>" . copilot-accept-completion-by-line)
;;                ("<left>"  . copilot-clear-overlay)
;;                ("RET"     . copilot-clear-overlay))))

;; 
;; Python API: sthenno-endpoints Client
;;
;; [TODO]
;; - https://github.com/alphapapa/plz.el
;; - https://github.com/karthink/gptel
;;

;; (require 'url)
;; (require 'json)

;; (setq sthenno-endpoint-u "http://localhost:8000")

;; (defun sthenno-post (endpoint content)
;;   "Send a json formatted post request to the specified ENDPOINT using CONTENT."
;;   (let* ((url-request-method "POST")
;;          (url-request-extra-headers '(("Content-Type" . "application/json")))
;;          (url-request-data (encode-coding-string
;;                             (json-encode `(("content" . ,content))) 'utf-8))
;;          (url (format "%s/%s/" sthenno-endpoint-u endpoint))
;;          response)
;;     (with-current-buffer (url-retrieve-synchronously url t) ; t means silent
;;       (goto-char url-http-end-of-headers)
;;       (setq response (buffer-substring-no-properties (point) (point-max)))
;;       (kill-buffer (current-buffer)))
;;     (json-read-from-string (decode-coding-string response 'utf-8))))

;; (defun sthenno-trans-to-zh (content)
;;   "Translate the CONTENT to Simplified Chinese.
;; Return the translation."
;;   (let* ((obj (sthenno-post "trans_to_zh" content))
;;          (translation (cdr (assoc 'translation obj))))
;;     translation))

;; (defun sthenno-trans-to-en (content)
;;   "Translate the CONTENT to English.
;; Return the translation."
;;   (let* ((obj (sthenno-post "trans_to_en" content))
;;          (translation (cdr (assoc 'translation obj))))
;;     translation))

;; (defun sthenno-trans-to-zh-target (target)
;;   (let ((translation (sthenno-trans-to-zh target)))
;;     (kill-new translation)
;;     (message (format "Translation: %s" translation))))

;; (defun sthenno-trans-to-en-target (target)
;;   (let ((translation (sthenno-trans-to-en target)))
;;     (kill-new translation)
;;     (message (format "Translation: %s" translation))))

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
