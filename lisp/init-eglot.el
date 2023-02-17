;;; init-eglot.el --- LSP support by eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  (setq eglot-autoshutdown t))


(require-package 'treesit-auto)
(global-treesit-auto-mode 1)


;; C/C++ support
(require 'cc-mode)

(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)

(add-hook 'c++-mode-hook #'eglot-ensure)

(defun pes-compile--cc-file ()
  (interactive)
  (compile (format "g++-12 -o %s %s -g -lm -Wall"
                   (file-name-sans-extension (buffer-name))
                   (buffer-name))))

(define-key c++-mode-map [f9] 'pes-compile--cc-file)


;; Python support
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'python-ts-mode
  (defun pes-run-current-python-file ()
    "Run the current Python file."
    (interactive)
    (python-shell-send-buffer))
  (define-key python-mode-map [f9] 'pes-run-current-python-file))

;; Python executable file location
(setq-default python-interpreter "python3.9")
(setq-default python-shell-interpreter "python3.9")
(setq-default org-babel-python-command "python3.9")

;; Ignore the warnings
(setq-default python-indent-guess-indent-offset t)
(setq-default python-indent-guess-indent-offset-verbose nil)

(define-key global-map (kbd "M-p r") 'run-python)


(provide 'init-eglot)
;;; init-eglot.el ends here
