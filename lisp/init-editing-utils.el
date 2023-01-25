;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-indent-mode))


;; Some basic preferences
(setq-default case-fold-search t)
(setq-default line-number-mode nil)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq-default create-lockfiles nil)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default indent-tabs-mode nil)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default mark-even-if-inactive nil)
(setq-default mouse-yank-at-point t)
(setq-default ring-bell-function 'ignore)
(setq-default save-interprogram-paste-before-kill t)
(setq-default save-silently t)
(setq-default set-mark-command-repeat-pop t)
(setq-default sentence-end-double-space nil)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq-default use-short-answers t)
(setq-default help-window-select t) ; Use `q' to close the help window


;; Formatting files
;; Add a new line in the end of buffer while saving
(setq-default require-final-newline t)

;; Format current buffer while saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'(lambda ()
                                (indent-region (point-min) (point-max) nil)))


;; Formatting buffers
(defun my/indent-and-save-buffer ()
  "Indent current buffer then save it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

(global-set-key (kbd "s-i") #'my/indent-and-save-buffer)


;; Enable the fundamental modes
(add-hook 'after-init-hook #'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook #'(lambda ()
                               (global-hl-line-mode 1)
                               (transient-mark-mode 1)
                               (save-place-mode 1)))


;; Fill columns
(setq-default fill-column 80)

(when (boundp 'display-fill-column-indicator)
  (add-hook 'prog-mode-hook #'(lambda ()
                                (display-fill-column-indicator-mode 1))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'(lambda ()
                               (visual-line-mode 1)
                               (diminish 'visual-line-mode))))


;; Deleting
(add-hook 'after-init-hook #'(lambda ()
                               (delete-selection-mode 1)))

(global-set-key (kbd "s-<backspace>") #'kill-whole-line)

(use-package smart-hungry-delete
  :init
  (smart-hungry-delete-add-default-hooks)
  :bind
  (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap delete-char] . smart-hungry-delete-forward-char)))


;; Newline behaviours
(global-set-key (kbd "RET") #'newline-and-indent)

(defun my/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") #'my/newline-at-end-of-line)


;; Improve displaying
;; The nano style for truncated long lines
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Display line numbers
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))

;; Enhance the performace of display
(setq display-raw-bytes-as-hex t)
(setq redisplay-skip-fontification-on-input t)


;; Use rainbow delimiters
(use-package rainbow-delimiters
  :demand t
  :config
  (add-hook 'prog-mode-hook #'(lambda ()
                                (rainbow-delimiters-mode 1))))



;; Always show the pointer's position
(setq-default make-pointer-invisible nil)

;; Hide cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; Preserve contents of system clipboard
(setq-default save-interprogram-paste-before-kill t)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
