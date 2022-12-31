;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package modus-themes
  :init
  (setq-default modus-vivendi-palette-overrides
                `(
                  (fringe unspecified)
                  ;; From the section "Make the mode line borderless"
                  (border-mode-line-active unspecified)
                  (border-mode-line-inactive unspecified)

                  ;; From the section "Make matching parenthesis more or less intense"
                  (bg-paren-match bg-magenta-intense)
                  (underline-paren-match fg-main)))
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
