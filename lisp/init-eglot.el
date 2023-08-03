;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup `treesit'
(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize `eglot'
(use-package eglot
  :ensure t
  :defer t
  :config
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode #'eglot-ensure)
  :bind
  ((:map python-mode-map
         ("s-i" . eglot-format-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Debugging (TODO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;
;; Python environment management
;;
;;
;; Configure `conda' environment
(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/")
  (setq conda-env-home-directory
        (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/envs/"))
  (setq conda-env-autoactivate-mode t)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; TODO
(setenv "TG_TOKEN" "5754204406:AAGMzH4pXoEEBxgmu_EduH9zIhZmrne29tc")
(setq python-shell-interpreter "/opt/homebrew/Caskroom/miniconda/base/envs/gussie_telegram_bot/bin/python"
      python-shell-interpreter-args "-i"
      python-shell--interpreter python-shell-interpreter
      python-shell--interpreter-args python-shell-interpreter-args
      python-shell-prompt-detect-failure-warning nil
      python-shell-completion-native-enable nil)

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(bind-keys :map python-ts-mode-map
           ("s-i" . eglot-format-buffer))

;; Code navigation, documentation lookup and completion for Python
(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish (anaconda-mode anaconda-eldoc-mode)
  :hook (python-ts-mode . anaconda-mode)
  :bind
  (:map anaconda-mode-map
        ("M-?" . anaconda-mode-show-doc)
        ("M-." . anaconda-mode-find-definitions)
        ("M-," . anaconda-mode-find-assignments)
        ("M-r" . anaconda-mode-find-references)
        ("M-*" . anaconda-mode-go-back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JavaScript and TypeScript (TODO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configure the ChatGPT client
(use-package gptel
  :ensure t
  :defer t
  :init
  (defun get-env-var-from-zshrc (var-name)
    "Return the value of the environment variable VAR-NAME in the .zshrc file."
    (let* ((command (concat "source ~/.zshrc; echo $" var-name))
           (output (shell-command-to-string command))
           (matched (string-match "\\(.*\\)\n" output)))
      (if matched
          (match-string 1 output)
        (error "Variable %s not found in .zshrc" var-name))))
  :config
  (setq gptel-api-key (lambda ()
                        (get-env-var-from-zshrc "OPENAI_API_KEY")))
  (setq gptel-default-mode #'org-mode)
  :bind
  ("C-x c" . gptel))

(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
