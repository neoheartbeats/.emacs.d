;;; init-fonts.el --- Setup fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :font "Pes Mono"
	            :height 140)

(set-fontset-font "fontset-default" 'unicode "SF Pro")
(set-fontset-font "fontset-default" 'han "HarmonyOS Sans SC")

(with-eval-after-load 'init-themes ; Remove italic constructs
  (set-face-attribute 'italic nil
                      :slant 'normal))


(provide 'init-fonts)
;;; init-fonts.el ends here
