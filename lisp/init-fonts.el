;;; init-fonts.el --- Configure the fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :font "Nodens"
	            :height 145)

(set-face-attribute 'variable-pitch nil
                    :font "Noto Serif CJK SC")

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "Noto Serif CJK SC")
(set-fontset-font "fontset-default" 'kana "Noto Serif CJK SC")


;; No gc for font caches
(setq inhibit-compacting-font-caches t)


;; Enable font ligatures (only in emacs-mac)
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode 1))


(provide 'init-fonts)
;;; init-fonts.el ends here
