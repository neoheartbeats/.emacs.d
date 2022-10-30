;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


;;; Modus Themes
;; (use-package modus-themes
;;   :init
;;   (modus-themes-load-themes)
;;   :custom
;;   (modus-themes-bold-constructs t)
;;   (modus-themes-inhibit-reload t)
;;   (modus-themes-subtle-line-numbers t)
;;   (modus-themes-fringes 'subtle)
;;   (modus-themes-syntax '(green-strings))
;;   (modus-themes-links '(neutral-underline no-color))
;;   (modus-themes-prompts '(intense bold))
;;   (modus-themes-region '(bg-only no-extend))
;;   (modus-themes-mode-line '(borderless))
;;   (modus-themes-hl-line '(accented underline))
;;   :config
;;   (modus-themes-load-vivendi))

(setq modus-themes-bold-constructs t)
(setq modus-themes-inhibit-reload t)
(setq modus-themes-subtle-line-numbers t)
(setq modus-themes-syntax '(green-strings))
(setq modus-themes-links '(no-color neutral-underline))
(setq modus-themes-prompts '(intense bold))
(setq modus-themes-region '(bg-only no-extend))
(setq modus-themes-mode-line '(borderless))

(load-theme 'modus-vivendi)


;;; Mode line settings
(setq-default mode-line-compact t)


;;; Ef Themes
;; (use-package ef-themes
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'ef-bio :no-confirm))


(provide 'init-themes)
;;; init-themes.el ends here
