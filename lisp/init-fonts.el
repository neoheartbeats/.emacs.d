;;; init-fonts.el --- Setup fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
	            :family "PragmataPro"
	            :height 140)

(set-face-attribute 'variable-pitch nil
                    :family "HarmonyOS Sans SC")

(set-fontset-font "fontset-default" 'unicode "PragmataPro")
(set-fontset-font "fontset-default" 'han "HarmonyOS Sans SC")

(with-eval-after-load 'init-themes ; Remove italic constructs
  (set-face-attribute 'italic nil
                      :slant 'normal))


(provide 'init-fonts)
;;; init-fonts.el ends here
