;; init-emms.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Emms config.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup Emms

(use-package emms
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  :config
  (defun emms-shuffle-start ()
    "Shuffle & start Emms in specified directory"
    (interactive)
    (emms-add-directory "~/org/Emms/")
    (emms-shuffle)
    (emms-start))
  :bind
  ("s-p s" . emms-shuffle-start)
  ("s-p n" . emms-next)
  ("s-p p" . emms-previous)
  ("s-p t" . emms-stop))

(provide 'init-emms)

;; init-emms.el ends here
