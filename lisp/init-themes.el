;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(use-package ef-themes
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-autumn :no-confirm))



;; Customize faces
(set-face-attribute 'button nil
                    :underline "#959595"
                    :foreground nil)

(set-face-attribute 'fill-column-indicator nil
                    :height 0.15)

(set-face-attribute 'link nil
                    :foreground nil)

(set-face-background 'fringe (face-attribute 'default :background))

;; Cursor faces
(setq-default blink-cursor-mode nil)
(setq-default cursor-type '(bar . 1))


;; Highlight defined Elisp symbols in source code
(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                      (highlight-defined-mode 1)))
  :custom-face
  (highlight-defined-face-name-face ((t :inherit org-block))))


;; Mode line settings
;; Use one single line as mode line
(use-package emacs
  :custom-face
  (mode-line ((t (:height 0.1))))
  (mode-line-inactive ((t (:inherit mode-line))))
  :config
  (setq-default mode-line-format '("")))


(provide 'init-themes)
;;; init-themes.el ends here
