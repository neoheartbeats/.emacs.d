;;; init-temp.el --- Modern template system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;

;;; Code:
;;

;;; Abbrevs

(use-package abbrev
  :init
  (setq abbrev-file-name (locate-user-emacs-file "abbrev-defs.el"))

  ;; Do not ask before saving abbrevs
  (setq save-abbrevs 'silently)

  ;; Hooks
  (add-hook 'text-mode-hook #'(lambda ()
                                (abbrev-mode 1)))
  (add-hook 'prog-mode-hook #'(lambda ()
                                (abbrev-mode 1)))
  :bind (:map global-map
              ("C-x a e" . edit-abbrevs)
              ("C-x a a" . add-global-abbrev)))

;;; YASnippet

;; (use-package yasnippet
;;   :ensure t
;;   :demand t
;;   :diminish (yas-minor-mode)
;;   :config
;;   (setq yas-triggers-in-field t)
;;   (setq yas-visit-from-menu t)
;;   (yas-global-mode 1))

;;; TempEl: Simple templates for Emacs

;; Configure Tempel

(use-package tempel
  :ensure t
  :init
  (setopt tempel-trigger-prefix "<")

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand' only
    ;; triggers on exact matches. Alternatively use `tempel-complete' if you want to see
    ;; all matches, but then you should also configure `tempel-trigger-prefix', such
    ;; that Tempel does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such that it will be
    ;; tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf)

  :bind (("TAB" . tempel-complete)))

;; Add 'tempel-collection'

(use-package tempel-collection
  :ensure t
  :after tempel)

;;; _
(provide 'init-temp)
