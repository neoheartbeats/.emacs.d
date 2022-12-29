;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package ef-themes
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark :no-confirm)

  (defun my/ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((
                     ,c :background
                     ,bg-active :foreground
                     ,fg-main :box (:line-width 1 :color ,fg-dim))))
       `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (with-eval-after-load 'init-themes
    (my/ef-themes-mode-line)))


;;; Mode line settings
(setq-default mode-line-compact t)


(provide 'init-themes)
;;; init-themes.el ends here
