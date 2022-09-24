;;; init-gui-frames.el --- Behaviour specific to GUI frames -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Stop C-z from minimizing windows under macOS
(global-set-key (kbd "C-z") 'suspend-frame)


;; Using `C-c C-f' to toggle fullscreen
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)


;; Default startup message
(defun display-startup-echo-area-message ()
  (message "Funding for this program was made possible by viewers like you."))

;; Always show the pointer's position
(setq make-pointer-invisible nil)


;; Cleanup the text & icons
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;;; Diminish prompt messages
;; Disable these messages such ignore unused signals
(defun filter-command-error-function (data context caller)
  (when (not (memq (car data) '(buffer-read-only
                                 beginning-of-line
                                 end-of-line
                                 beginning-of-buffer
                                 end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'filter-command-error-function)

;; Disable unnecessary warnings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


;; Non-zero values for `line-spacing' can mess up ansi-term and co
;; so we zero it explicitly in those cases
(add-hook 'term-mode-hook
  (lambda ()
    (setq line-spacing 0)))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
