;;; init-fonts.el --- Configure the fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	                  :font "LXGW Wenkai Mono"
	                  :height 155)

(set-face-attribute 'variable-pitch nil
                    :font "LXGW Wenkai")

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'emoji "Apple Color Emoji")
(set-fontset-font "fontset-default" 'han "LXGW Wenkai Mono")
(set-fontset-font "fontset-default" 'kana "LXGW Wenkai Mono")


(provide 'init-fonts)
;;; init-fonts.el ends here
