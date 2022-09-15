;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(use-package unfill)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)


;;; Some basic preferences
(setq-default
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 confirm-nonexistent-file-or-buffer nil
 cursor-in-non-selected-windows nil
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 save-place-mode t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)


(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(transient-mark-mode 1)


;; Replace "yes & no" with "y & n"
(fset 'yes-or-no-p 'y-or-n-p)
(define-key y-or-n-p-map [return] 'act)


(global-set-key (kbd "s-d") 'kill-line)


;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "s-<return>") 'sanityinc/newline-at-end-of-line)


(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


(use-package rainbow-delimiters
  :hook
  ((prog-mode org-mode) . rainbow-delimiters-mode))


;; Settings for cursors
(set-face-attribute 'cursor nil :background "#217a3c")
(blink-cursor-mode -1)


;; Show lambda as unicode
(global-prettify-symbols-mode 1)


;; Formatting code by EditorConfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
