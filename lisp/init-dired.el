;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :defer t
  :config
  (diredfl-global-mode 1)
  (require 'dired-x))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(use-package diff-hl
  :defer t
  :hook
  (dired-mode . diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
