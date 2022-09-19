;;; init-osx.el --- Configure keys specific to macOS -*- lexical-binding: t -*-
;;; Commentary:

;; macOS specific settings.

;;; Code:


;; Setup modifier keys
;; macOS specified key mapping
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; Enable scrolling smooth
;; Make scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                   ((shift) . 5)
                                   ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; Using `pixel-scroll-precision-mode'
(pixel-scroll-precision-mode 1)


;; Set up exec-path to help Emacs find programs
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))


;;; macOS styled keybindings
;; Editing specified
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "S-s-z") 'undo-redo)
(global-set-key (kbd "s-i") 'indent-region)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<s-d>") 'backward-kill-sentence)

;; Buffer specified
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-w") (lambda ()
															(interactive)
															(kill-buffer (current-buffer))))
(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

;; Window & frame specified
(global-set-key (kbd "s-e") 'delete-window)

;; File management specified
(global-set-key (kbd "s-n") 'find-file)
(global-set-key (kbd "C-c p") (lambda ()
																(interactive)
																(find-file user-init-file)))


;; Disable swipe left/right to change buffer
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])


(provide 'init-osx)
;;; init-osx.el ends here
