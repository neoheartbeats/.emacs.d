;; defaults.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Modified built-in functions.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS styled keybindings
;;
;; Editing
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
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

;; Window & frame management
(global-set-key (kbd "s-e") 'delete-window)
(global-set-key (kbd "s-m") 'toggle-frame-fullscreen)

;; File management
(global-set-key (kbd "s-n") 'find-file)
(global-set-key (kbd "C-c p") (lambda ()
																(interactive)
																(find-file "~/.emacs.d/init.el")))

;; Disable swipe left/right to change buffer
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance backups & caches
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Replace "yes & no" with "y & n"
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance editing
;;
;; Adjust indentings
;; (setq-default indent-tabs-mode t)
;; (setq-default tab-width 4)
;;
;; EditorConfig setup
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Make sentences divided by one space
(setq sentence-end-double-space nil)

;; Modern deleting style
(delete-selection-mode 1)
(setq shift-select-mode nil)

;; Create newline at end of file
(setq require-final-newline t)

;; View the whole line
(global-visual-line-mode 1)

;; Show line numbers in prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Show lambda as λ
(global-prettify-symbols-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font ligatures support
(setq mac-auto-operator-composition-characters "!\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~")
(mac-auto-operator-composition-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Diminish prompt messages
;;
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

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Inhibit tooltips
(tooltip-mode -1)

;; Remove cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffers setup
;;
;; Revert buffers when underlying files are changed externally
(global-auto-revert-mode t)

;; Set initial buffer mode to org-mode
(setq-default initial-major-mode 'org-mode)

;; Note cursor position for each buffer
(save-place-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup `dired'
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-use-ls-dired nil) ;; Fix error "ls does not support --dired"
  (dired-kill-when-opening-new-dired-buffer t))

(provide 'defaults)
