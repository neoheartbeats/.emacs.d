;;; init-fonts.el --- Setup fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :font "Credits"
	            :height 140)

(set-face-attribute 'variable-pitch nil
                    :font "Noto Serif CJK SC")

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")

(with-eval-after-load 'init-themes ; Remove italic constructs
  (set-face-attribute 'italic nil
                      :slant 'normal))


(provide 'init-fonts)
;;; init-fonts.el ends here
