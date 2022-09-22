;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Ef Themes.

;;; Code:

(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-inhibit-reload t)
  (modus-themes-links '(neutral-underline))
  (modus-themes-headings '((t . (rainbow))))
  :config
  (modus-themes-load-vivendi))


(provide 'init-themes)
;;; init-themes.el ends here
