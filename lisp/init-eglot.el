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
;;   :straight t
;;   :config (global-treesit-auto-mode 1))

;; Remap `python-mode' to `python-ts-mode'
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; To enable the maximum fontifications. If this is set to default, there could be
;; syntax highlighting error found in Org Babel
;;

(setq treesit-font-lock-level 4)


;;
;; Initialize `eglot'
;;

(use-package eglot
  :straight t
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

;; Speed up
(use-package eglot-booster
  :after (eglot)
  :straight (eglot-booster
             :type git
             :host github
             :repo "jdtsmith/eglot-booster")
  :init (add-to-list 'exec-path (expand-file-name "bin/" user-emacs-directory))
  :config
  (setq eglot-booster-no-remote-boost t)
  (eglot-booster-mode 1))

;; Automatically confirm .dir-locals.el files
(setq-default enable-local-variables :safe)


;; Python project management [TODO]
(use-package python
  :config (setq python-indent-offset 4))

;; Reformat Python buffers using the Black formatter
;; TODO: See `init-editing-utils'
(use-package blacken
  :straight t
  :init (add-hook 'python-ts-mode-hook #'(lambda ()
                                           (blacken-mode 1)))
  :bind (:map python-ts-mode-map
              ("s-i" . blacken-buffer)))


;;; AI Integration
;;
;; LLM client
;;
(use-package gptel
  :straight t
  :init
  (setq gptel-default-mode #'org-mode)

  ;; System messages
  (setq gptel-directives
        '((default . "You are a helpful assistant living in Emacs.")))

  ;; Generation options
  (setq gptel-max-tokens 240
        gptel-temperature 0.5)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Use Ollama as the default backend
  ;;
  (defun my-gptel-ollama-backend--host (host)
    "Make a function to initialize an Ollama backend using the given HOST."
    `(lambda (model)
       (gptel-make-ollama model
         :host ,host
         :stream t
         :models `(,model))))

  (defun my-gptel-ollama-backend--localhost (model)
    "Initialize an Ollama backend using localhost and the given MODEL."
    (funcall (my-gptel-ollama-backend--host "localhost:11434") model))

  ;; Setup the model
  (let* ((model "phi3")
         (backend (my-gptel-ollama-backend--localhost model)))
    (setq gptel-model model
          gptel-backend backend))

  ;; UI
  ;;
  ;; Scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; Move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  :config
  ;; HACK
  ;;
  (defvar loading-animation-chars '("." ".." "..." "...." ".....")
    "Characters used for loading animation.")

  (defvar loading-animation-timer nil
    "Timer object for loading animation.")

  (defun start-loading-animation (text)
    "Start the loading animation and TEXT in the echo area."
    (let ((i 0))
      (setq loading-animation-timer
            (run-at-time 0 0.2
                         (lambda ()
                           (let* ((char (nth (mod i (length loading-animation-chars))
                                             loading-animation-chars))
                                  (msg (format "%s %s" text char)))
                             (message msg)
                             (setq i (1+ i))))))))

  (defun stop-loading-animation ()
    "Stop the loading animation."
    (when loading-animation-timer
      (cancel-timer loading-animation-timer)
      (setq loading-animation-timer nil)))

  ;; Add loading message to `gptel-send'
  ;;
  (defun my-gptel--loading-msg (loading-msg)
    "Propertize LOADING-MSG using specified `text-properties'."
    (modus-themes-with-colors
      (let ((msg (propertize loading-msg
                             'face `(:foreground ,magenta-intense :inherit 'bold))))
        msg)))

  (defun my-gptel-send-querying-loading ()
    "Now-loading behavior of `gptel-send' during querying."
    (let ((msg (my-gptel--loading-msg
                (format "✿ 少女祈祷中 􀍠 [%s]"
                        (gptel-backend-name gptel-backend)))))
      (start-loading-animation msg)))

  (defun my-gptel-send-insert-loading ()
    "Now-loading behavior of `gptel-send' during inserting."
    (let ((msg (my-gptel--loading-msg "􁄤 少女响应中 􀍠")))
      (stop-loading-animation)
      (message msg)))

  (add-hook 'gptel-pre-response-hook #'my-gptel-send-insert-loading)

  ;; HACK
  (defun gptel-send (&optional arg)
    "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
    (interactive "P")
    (if (and arg (require 'gptel-transient nil t))
        (call-interactively #'gptel-menu)
      (my-gptel-send-querying-loading)
      (gptel--sanitize-model)
      (gptel-request nil :stream gptel-stream)
      (gptel--update-status " Waiting..." 'warning)))

  ;; Functions of the `gptel' buffer
  ;;
  (add-hook 'gptel-mode-hook #'turn-on-visual-line-mode)

  (defun my-gptel-to-buffer ()
    "Open the gptel buffer."
    (interactive)
    (let ((buff "*Φ*"))
      (gptel buff)

      ;; TODO: Add repeat keys to jump back
      (switch-to-buffer buff)
      (message "Ciallo～(∠・ω< )⌒☆")))

  (global-set-key (kbd "s-l") #'my-gptel-to-buffer)

  :bind (:map gptel-mode-map
              ("s-<return>" . gptel-send)))

;; GitHub Copilot
;;

;; (use-package copilot
;;   :straight (
;;              :host github
;;              :repo "copilot-emacs/copilot.el"
;;              :files ("*.el"))
;;   :config (define-key global-map (kbd "s-.") #'copilot-accept-completion))


;;; Python API: sthenno-endpoints Client
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
