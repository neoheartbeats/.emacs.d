;;; init-uniquify.el --- Configure uniquification of buffer names -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Nicer naming of buffers for files with identical names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)
;;; init-uniquify.el ends here
