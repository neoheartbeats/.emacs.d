;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


;;; Modus Themes
(setq modus-themes-bold-constructs t)
(setq modus-themes-inhibit-reload t)
(setq modus-themes-subtle-line-numbers t)
(setq modus-themes-syntax '(green-strings))
(setq modus-themes-links '(neutral-underline))
(setq modus-themes-prompts '(intense bold))
(setq modus-themes-mode-line '(borderless))

(load-theme 'modus-vivendi t)


;;; Mode line settings
(setq-default mode-line-compact t)


;;; Ef Themes
;; (use-package ef-themes
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'ef-bio :no-confirm))


(provide 'init-themes)
;;; init-themes.el ends here
