;;; init-gui-frames.el --- Behaviour specific to GUI frames -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Stop C-z from minimizing windows under macOS
(global-set-key (kbd "C-z") 'suspend-frame)


;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)


;; Pixelwise resize windows
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(top . 155))
(add-to-list 'default-frame-alist '(width . 105))
(add-to-list 'default-frame-alist '(height . 35))


;; Using `C-c C-f' to toggle fullscreen
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)


;;; Title bar settings
;; Using modern title bar faces
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))


;; Default startup message
(defun display-startup-echo-area-message ()
  (message "Welcome to Emacs!"))

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
