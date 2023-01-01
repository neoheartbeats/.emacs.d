;;; init-gui-frames.el --- Behaviour specific to GUI frames -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; Using `C-c f' to toggle fullscreen
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)


;; Default startup message
(defun display-startup-echo-area-message ()
  (message ""))


;; Cleanup the text & icons
(setq-default ns-use-proxy-icon nil)
(setq-default frame-title-format nil)


(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Set frame border width
(modify-all-frames-parameters
 '((right-divider-width . 5)
   (internal-border-width . 5)))

;; Stop showing fringe bitmaps
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil))


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


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
