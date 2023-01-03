;;; init-gui-frames.el --- Behaviour specific to GUI frames -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Using `C-c f' to toggle fullscreen
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)


;; Default startup message
(defun display-startup-echo-area-message () (message ""))


;; Cleanup the text & icons
(setq-default ns-use-proxy-icon nil)
(setq-default frame-title-format nil)

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
