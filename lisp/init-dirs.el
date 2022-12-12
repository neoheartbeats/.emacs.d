;;; init-dirs.el --- File management in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides functions for file management based on 'dirvish`
;; and additionally provides file applications support.

;;; Code:


;; File management with `dirvish'
(use-package dirvish
  :init
  (dirvish-override-dired-mode 1))


(provide 'init-dirs)
;;; init-dirs.el ends here
