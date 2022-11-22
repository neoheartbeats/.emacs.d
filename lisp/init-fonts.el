;;; init-fonts.el --- Configure the fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :font "PragmataPro Mono Liga"
	            :height 145)

(set-face-attribute 'variable-pitch nil
                    :font "HarmonyOS Sans SC")

(set-fontset-font "fontset-default" 'unicode "PragmataPro")
(set-fontset-font "fontset-default" 'han "HarmonyOS Sans SC")
(set-fontset-font "fontset-default" 'kana "HarmonyOS Sans SC")


;; No gc for font caches
(setq inhibit-compacting-font-caches t)


(provide 'init-fonts)
;;; init-fonts.el ends here
