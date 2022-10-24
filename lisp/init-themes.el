;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Modus/Ef Themes.

;;; Code:


;;; Modus Themes
(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-inhibit-reload t)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-fringes 'subtle)
  (modus-themes-syntax '(green-strings))
  (modus-themes-links '(neutral-underline no-color))
  (modus-themes-prompts '(intense bold))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-mode-line '(borderless))
  (modus-themes-hl-line '(accented underline))
  :config
  (modus-themes-load-vivendi))


;;; Ef Themes
;; (use-package ef-themes
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'ef-bio :no-confirm))


(provide 'init-themes)
;;; init-themes.el ends here
