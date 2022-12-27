;;; init-dirs.el --- File management in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides functions for file management based on 'dirvish`
;; and additionally provides file applications support.

;;; Code:


;; File management with `dirvish'
(use-package dirvish
  :defer t
  :init
  (dirvish-override-dired-mode 1)
  (global-unset-key (kbd "s-d"))
  :bind
  (("s-d d" . dirvish)))


(provide 'init-dirs)
;;; init-dirs.el ends here
