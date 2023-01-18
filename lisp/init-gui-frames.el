;;; init-gui-frames.el --- Behaviour specific to GUI frames -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Using `C-c f' to toggle fullscreen
(global-set-key (kbd "C-c f") #'toggle-frame-fullscreen)


;; Default startup message
(defun display-startup-echo-area-message ()
  (message ""))


;; Set the text and icons in title
(setq-default ns-use-proxy-icon nil)
(setq-default frame-title-format nil)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))


;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases
(add-hook 'term-mode-hook #'(lambda ()
                              (setq line-spacing 0)))


;; Nicer naming of buffers for files with identical names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " - ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
