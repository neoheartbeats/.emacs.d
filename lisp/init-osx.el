;;; init-osx.el --- Configure keys specific to macOS -*- lexical-binding: t -*-
;;; Commentary:

;; macOS specific settings.

;;; Code:


;; macOS specified key mapping
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)


;; Enable `pixel-scroll-precision-mode' (only in Emacs@29)
(cond ((version= emacs-version "29.0.50")
       (pixel-scroll-precision-mode 1)))


;; Set up `exec-path' to help Emacs find programs
;; (cond ((version= emacs-version "29.0.50")
;;        (use-package exec-path-from-shell
;;          :init
;;          (exec-path-from-shell-initialize))))


;; Disable auto copying
(setq mouse-drag-copy-region nil)

;; Disabe default key for scaling text
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))


;;; macOS styled keybindings
;; Editing specified
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "S-s-z") 'undo-redo)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Buffer specified
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-e") 'delete-window)
(global-set-key (kbd "s-w") (lambda ()
			      (interactive)
			      (kill-buffer (current-buffer))))
(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

;; File management specified
(global-set-key (kbd "s-n") 'find-file)
(global-set-key (kbd "C-c p") (lambda ()
				(interactive)
				(find-file user-init-file)))


;; Disable these keys
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])
(global-unset-key (kbd "C-z"))


(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))


;; Speedup `regexp' searching
(setq xref-search-program 'ripgrep)


(provide 'init-osx)
;;; init-osx.el ends here
