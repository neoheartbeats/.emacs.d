;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Eglot setup
;; (unless (not *const-q*)
;;   (package-install 'eglot))
;;
;; (use-package eglot
;;   :config
;;   (setq eglot-autoshutdown t)
;;   (setq read-process-output-max (* 1024 1024))
;;   :bind
;;   ((:map eglot-mode-map
;;          ("s-i" . 'eglot-format-buffer))))


(when (treesit-available-p)
  (push '(sh-mode . bash-ts-mode) major-mode-remap-alist)
  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))


;; C/C++ support
;; (require 'cc-mode)

;; (setq-default c-default-style "k&r")
;; (setq-default c-basic-offset 4)

;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; (defun my/compile--cc-file ()
;;   (interactive)
;;   (compile (format "g++-12 -o %s %s -g -lm -Wall"
;;                    (file-name-sans-extension (buffer-name))
;;                    (buffer-name))))
;; (define-key c++-mode-map [f9] 'my/compile--cc-file)


;; Python support
;; Enable `eglot' support
;; (add-hook 'python-mode-hook #'eglot-ensure)

;; (with-eval-after-load 'python-mode
;;   (defun my/run-current-python-file ()
;;     "Run the current Python file."
;;     (interactive)
;;     (python-shell-send-buffer))
;;   (define-key python-mode-map [f9] 'my/run-current-python-file))

;; Python executable file location
(setq-default python-interpreter "python3")
(setq-default python-shell-interprete "python3")
(with-eval-after-load 'org
  (setq-default org-babel-python-command "python3"))

;; ;; Ignore the warnings
;; (setq-default python-indent-guess-indent-offset t)
;; (setq-default python-indent-guess-indent-offset-verbose nil)

;; (global-set-key (kbd "M-p r") 'run-python)

(use-package chatgpt
  :straight (
             :host github
             :repo "joshcho/ChatGPT.el"
             :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
  :bind ("M-." . chatgpt-query))


(provide 'init-eglot)
;;; init-eglot.el ends here
