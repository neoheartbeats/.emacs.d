;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


;;; Modus Themes
(use-package emacs
  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-syntax '(green-strings))
  (setq modus-themes-links '(no-color))
  (setq modus-themes-prompts '(intense bold))
  (setq modus-themes-mode-line '(borderless))
  (setq modus-themes-region '(bg-only no-extend))
  (setq modus-themes-headings '((t . (rainbow))))
  :config
  (load-theme 'modus-vivendi t))


;;; Mode line settings
(setq-default mode-line-compact t)


;;; Ef Themes
;; (use-package ef-themes
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'ef-dark :no-confirm))


(provide 'init-themes)
;;; init-themes.el ends here
