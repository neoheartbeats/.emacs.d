;;; init-projects.el --- Project management -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:
;;

;; Git client using Magit
(use-package magit
  :straight t
  :config
  (setq magit-section-visibility-indicator '(" 􀰌"))
  (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))

;; (use-package diff-hl
;;   :straight t
;;   :config (global-diff-hl-mode 1))


;; Xref
;;
;; For further backends support, see also `etags', `init-eglot'
(use-package xref
  :init (setq xref-search-program 'ripgrep)
  :bind (:map global-map
              ("M-/" . xref-find-references)))

;; TAGS [FIXME]
;; (use-package etags
;;   :init
;;   (setq etags-program-name "/opt/homebrew/bin/etags")
;;   (add-hook 'emacs-lisp-mode-hook #'(lambda ()
;;                                       (etags-regen-mode 1))))

(provide 'init-projects)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
