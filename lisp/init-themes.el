;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package modus-themes
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-vivendi :no-confim))


;; Customize faces
(set-face-background 'line-number nil)
(set-face-background 'line-number-current-line nil)

(custom-set-faces
 '(org-link ((t (:inherit nil)))))


;;; Mode line settings
(setq-default mode-line-compact t)

(diminish 'eldoc-mode)


(provide 'init-themes)
;;; init-themes.el ends here
