;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Ef Themes.

;;; Code:

(use-package ef-themes
  :custom
  (ef-themes-headings '((0 . (variable-pitch))))
  (ef-themes-variable-pitch-ui t)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark :no-confirm))


(provide 'init-themes)
;;; init-themes.el ends here
