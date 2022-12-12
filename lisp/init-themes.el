;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


;;; Modus Themes
(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-syntax '(green-strings))
  (setq modus-themes-links '(no-color neutral-underline))
  (setq modus-themes-prompts '(intense bold))
  (setq modus-themes-mode-line '(borderless))
  (setq modus-themes-region '(bg-only no-extend))
  (setq modus-themes-headings '((t . (rainbow))))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  (set-face-attribute 'cursor nil
                      :background (modus-themes-color 'magenta-intense)))


;;; Mode line settings
(setq-default mode-line-compact t)


(provide 'init-themes)
;;; init-themes.el ends here
