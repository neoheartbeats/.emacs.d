;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(use-package unfill)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)


;; Some basic preferences
(setq-default case-fold-search t)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq-default create-lockfiles nil)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default indent-tabs-mode nil)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default mark-even-if-inactive nil)
(setq-default ring-bell-function 'ignore)
(setq-default save-interprogram-paste-before-kill t)
(setq-default save-silently t)
(setq-default set-mark-command-repeat-pop t)
(setq-default sentence-end-double-space nil) ; Sentences show end with one space
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq-default use-short-answers t)
(setq-default help-window-select t) ; Use `q' to close the help window


;; Essential key bindings
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)


;;; Formatting files
;; Add a new line in the end of buffer while saving
(setq-default require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default fill-column 95)


(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "S-s-z") 'undo-redo)


;; Enable those features
(dolist (c '(narrow-to-region
	     narrow-to-page
	     upcase-region
	     downcase-region))
  (put c 'disabled nil))
(put 'overwrite-mode 'disabled t)


;; Enable the fundamental modes
(global-auto-revert-mode 1)
(transient-mark-mode 1)
(save-place-mode 1)
(global-hl-line-mode 1)

(add-hook 'org-mode-hook 'visual-line-mode)


;;; Deleting
(delete-selection-mode 1)
(use-package smart-hungry-delete
  :bind
  (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap delete-char] . smart-hungry-delete-forward-char))
  :init
  (smart-hungry-delete-add-default-hooks))


;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun my/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "s-<return>") 'my/newline-at-end-of-line)


;;; Improve displaying
;; The nano style for truncated long lines
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Display line numbers while in `prog-mode'
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enhance the performace of display
(setq display-raw-bytes-as-hex t)
(setq redisplay-skip-fontification-on-input t)


;; Use rainbow delimiters
(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode . rainbow-delimiters-mode))


;;; Settings for cursors & clipboard
;; Make the cursor solid
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))

;; Always show the pointer's position
(setq make-pointer-invisible nil)

;; Hide cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)


;; Show lambda as unicode
(add-hook 'prog-mode-hook 'prettify-symbols-mode)


;; Formatting buffers
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "s-i") 'indent-buffer)


;; Highlight selected contents
(use-package highlight-thing
  :diminish (highlight-thing-mode)
  :hook
  (prog-mode . highlight-thing-mode))


;;; Literature related
(setq save-abbrevs 'silent)
(add-hook 'org-mode-hook 'abbrev-mode)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
