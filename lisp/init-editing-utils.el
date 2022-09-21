;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(use-package unfill)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)


;; Some basic preferences
(setq-default
  bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
  case-fold-search t
  column-number-mode t
  confirm-nonexistent-file-or-buffer nil
  create-lockfiles nil
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  fill-column 75
  indent-tabs-mode nil
  auto-save-default nil
  make-backup-files nil
  mark-even-if-inactive nil
  mouse-yank-at-point t
  ring-bell-function 'ignore
  save-interprogram-paste-before-kill t
  save-silently t
  scroll-preserve-screen-position 'always
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil
  use-short-answers t)


;; Enable those features
(dolist (c '(narrow-to-region
              narrow-to-page
              upcase-region
              downcase-region))
  (put c 'disabled nil))
(put 'overwrite-mode 'disabled t)


;; Enable the fundamental modes
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(transient-mark-mode 1)
(save-place-mode 1)


;; Useful keys
(global-set-key (kbd "s-d") 'kill-line)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") (lambda ()
                                   (goto-char (point-max))))


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
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Enhance the performace of display
(setq display-raw-bytes-as-hex t
  redisplay-skip-fontification-on-input t)


;; Use rainbow delimiters
(use-package rainbow-delimiters
  :hook
  ((prog-mode org-mode) . rainbow-delimiters-mode))


;;; Settings for cursors & clipboard
;; Make the cursor solid
(blink-cursor-mode -1)
(set-cursor-color "#217a3c")

;; Fancy cursor condition indication
;; This function can also help correct displaying
;; `prettify-symbols-mode'
(use-package beacon
  :diminish
  :config
  (beacon-mode 1))

;; Hide cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)


;; Show lambda as unicode
(global-prettify-symbols-mode 1)


;;; Auto editing settings
;; Formatting code by EditorConfig
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
