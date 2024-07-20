;;; init-projects.el --- Project management -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:
;;

(use-package project
  :init
  (setq project-prompter #'project-prompt-project-name)

  (defun project-magit-status ()
    "Run magit-status in the current project's root."
    (interactive)
    (magit-status-setup-buffer (project-root (project-current t))))

  :bind (:map global-map
              ("C-x p <backspace>" . sthenno/project-remove-project)))


;; Git client using Magit
(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-section-visibility-indicator '(" 􀰌"))
  (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))


;;; Xref
;;
;; For further backends support, see also `etags', `init-eglot'
;;
(use-package xref
  :init (setq xref-search-program 'ripgrep)
  :bind (:map global-map
              ("M-/" . xref-find-references)))

;; TAGS
;; (use-package etags
;;   :init

;;   ;; Automatically generate and update tags tables
;;   (add-hook 'emacs-lisp-mode-hook #'(lambda ()
;;                                       (etags-regen-mode 1))))

(provide 'init-projects)
