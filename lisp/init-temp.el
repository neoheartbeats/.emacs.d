;;; init-temp.el --- Modern template system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
;;

;;; Abbrevs

(use-package abbrev
  :diminish
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

(use-package yasnippet
  :ensure t
  :demand t
  :diminish (yas-minor-mode)
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(provide 'init-temp)
;;; init-temp.el ends here.
