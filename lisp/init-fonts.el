;;; init-fonts.el --- Configure the fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	                  :font "Myosotis"
	                  :height 155)

(set-face-attribute 'variable-pitch nil
                    :font "Futura")

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'emoji "Apple Color Emoji")
(set-fontset-font "fontset-default" 'han "HarmonyOS Sans SC")
(set-fontset-font "fontset-default" 'kana "HarmonyOS Sans SC")


;; No gc for font caches
(setq inhibit-compacting-font-caches t)


(provide 'init-fonts)
;;; init-fonts.el ends here
