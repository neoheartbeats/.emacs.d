;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Modus Themes.

;;; Code:

(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-inhibit-reload t)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-fringes nil)
  (modus-themes-syntax '(green-strings))
  (modus-themes-links '(neutral-underline no-color))
  (modus-themes-prompts '(intense bold))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-hl-line '(accented))
  (modus-themes-mode-line '(borderless))
  (modus-themes-headings '((t . (rainbow))))
  :config
  (modus-themes-load-operandi))


(provide 'init-themes)
;;; init-themes.el ends here
