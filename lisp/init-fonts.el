;;; init-fonts.el --- Setup fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :font "Credits"
	            :height 150)

(with-eval-after-load 'org
  (set-fontset-font "fontset-default" 'han "Heiti SC"))

(with-eval-after-load 'init-themes ;; Remove italic constructs
  (set-face-attribute 'italic nil
                      :slant 'normal))


(provide 'init-fonts)
;;; init-fonts.el ends here
