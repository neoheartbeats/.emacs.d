;; defaults.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Essentials must be loaded first.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS styled keybindings
;;
;; Editing
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "S-s-z") 'undo-redo)
(global-set-key (kbd "s-i") 'indent-region)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<s-escape>") 'backward-kill-sentence)

;; Buffer management
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-w") (lambda ()
			      (interactive)
			      (kill-buffer (current-buffer))))
(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

;; Window management
(global-set-key (kbd "s-e") 'delete-window)
(global-set-key (kbd "s-m") 'toggle-frame-fullscreen)

;; File management
(global-set-key (kbd "s-n") 'find-file)
(global-set-key (kbd "C-c p") (lambda ()
				(interactive)
				(find-file "~/.emacs.d/init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance backups & caches
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Replace "yes & no" with "y & n"
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance editing
;;
;; Adjust indentations
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Make sentences divided by one space
(setq sentence-end-double-space nil)

;; Modern deleting style
(delete-selection-mode 1)
(setq shift-select-mode nil)

;; Highting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Create newline at end of file
(setq require-final-newline t)

;; View the whole line
(global-visual-line-mode 1)

;; Show line numbers in prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Show lambda as λ
(defun prog-icons ()
  (setq prettify-symbols-alist
	'(("lambda" . "λ")))
  (prettify-symbols-mode))
(add-hook 'prog-mode-hook 'prog-icons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Diminish prompt messages
;;
;; Make it quiet
(setq visible-bell t)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq pop-up-windows nil)

;; Disable unnecessary warnings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Inhibit default messages
(setq inhibit-startup-message t)

;; Start without default scratch buffer
(setq initial-buffer-choice nil)

;; Clean frame title
(setq frame-title-format nil)

;; Inhibit tooltips
(tooltip-mode -1)

;; Remove cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default init modes
;;
;; Default init mode to text mode
(setq initial-major-mode 'text-mode)

;; Default major mode to text mode
(setq default-major-mode 'text-mode)

(provide 'defaults)

;; defaults.el ends here
