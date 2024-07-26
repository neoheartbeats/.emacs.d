;;; init-temp.el --- Modern template system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;

;;; Code:
;;

;; Abbrevs
(use-package abbrev
  :init
  (setq abbrev-file-name (expand-file-name "abbrev-defs.el" user-emacs-directory))

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

;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (setq yas-triggers-in-field t)
  (setq yas-visit-from-menu t)
  (yas-global-mode 1))

(provide 'init-temp)
