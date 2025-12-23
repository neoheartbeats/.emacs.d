;;; init-temp.el --- Modern template system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
;;

;;; Abbrevs

;; (use-package abbrev
;;   :diminish
;;   :init
;;   (setq abbrev-file-name (locate-user-emacs-file "abbrev-defs.el"))

;;   ;; Do not ask before saving abbrevs
;;   (setq save-abbrevs 'silently)

;;   ;; Hooks
;;   (add-hook 'text-mode-hook #'(lambda ()
;;                                 (abbrev-mode 1)))
;;   (add-hook 'prog-mode-hook #'(lambda ()
;;                                 (abbrev-mode 1)))
;;   :bind (:map global-map
;;               ("C-x a e" . edit-abbrevs)
;;               ("C-x a a" . add-global-abbrev)))

;;; YASnippet
(use-package yasnippet
  :ensure t
  :defer t
  :diminish (yas-minor-mode)
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

;;; TempEl - Simple templates
;; (use-package tempel
;;   :ensure t
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "<")

;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert))

;;   :init

;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))

;;   (add-hook 'conf-mode-hook 'tempel-setup-capf)
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf)

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;;   )

(provide 'init-temp)

;;; init-temp.el ends here
