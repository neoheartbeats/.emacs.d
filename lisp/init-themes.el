;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:

;; This file configured Emacs Themes.

;;; Code:


(load-theme 'modus-vivendi t)


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
(setq-default mode-line-compact t)


(provide 'init-themes)
;;; init-themes.el ends here
