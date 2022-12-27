;;; init-gui-frames.el --- Behaviours of GUI frames -*- lexical-binding: t -*-
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


(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'."
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions #'run-after-make-frame-hooks)

(defconst my/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          #'(lambda () (when my/initial-frame
                         (run-after-make-frame-hooks my/initial-frame))))


(setq switch-to-buffer-obey-display-actions t)


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
