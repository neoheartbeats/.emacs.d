;; init-comp.el ---  Modern template system -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configure Tempel
(use-package tempel
  :straight t
  :bind
  (
    ("s-." . tempel-expand)
    ("s-/" . tempel-insert))
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    "Add the Tempel Capf to `completion-at-point-functions'."
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
        completion-at-point-functions)))

  (add-hook 'org-mode-hook #'tempel-setup-capf)
  :config
  (setq tempel-path "/Users/ilyaw39/.emacs.d/temp.eld"))


(provide 'init-temp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-temp ends here
