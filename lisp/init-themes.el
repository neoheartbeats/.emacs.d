;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package modus-themes
  :init
  (setq modus-themes-region '(no-extend))
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-vivendi :no-confim)

  ;; Customize Org links
  (custom-set-faces '(org-link ((t (:inherit nil))))))


;;; Mode line settings
(setq-default mode-line-compact t)

;; Diminishing components
(use-package diminish
  :config
  (diminish 'eldoc-mode))


(provide 'init-themes)
;;; init-themes.el ends here
