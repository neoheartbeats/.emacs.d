;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'electric-indent-mode))

(setq-default indent-tabs-mode nil)


;; Some basic preferences
(setq-default case-fold-search t)
(setq-default create-lockfiles nil)
(setq-default ediff-split-window-function 'split-window-horizontally)
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
(add-hook
 'before-save-hook
 #'(lambda ()
     (delete-trailing-whitespace)
     (indent-region (point-min) (point-max) nil)))

(when (maybe-require-package 'elisp-autofmt)
  (setq elisp-autofmt-python-bin my-python-exec-path)
  (add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode)
  (define-key emacs-lisp-mode-map (kbd "s-i") 'elisp-autofmt-buffer))

;; Formatting buffers
(defun pes-indent-and-save-buffer ()
  "Indent current buffer then save it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

(global-set-key (kbd "s-i") #'pes-indent-and-save-buffer)


;; Enable the fundamental modes

(add-hook 'after-init-hook #'delete-selection-mode)

(add-hook 'after-init-hook #'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))


;; Fill columns
(setq-default fill-column 80)

(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


;; Deleting
(global-set-key (kbd "s-<backspace>") #'kill-whole-line)

(when (maybe-require-package 'smart-hungry-delete)
  (smart-hungry-delete-add-default-hooks)
  (with-eval-after-load 'smart-hungry-delete
    (define-key
     global-map
     [remap backward-delete-char-untabify]
     'smart-hungry-delete-backward-char)
    (define-key
     global-map [remap delete-backward-char] 'smart-hungry-delete-backward-char)
    (define-key
     global-map [remap delete-char] 'smart-hungry-delete-forward-char)))


;; Newline behaviours
(global-set-key (kbd "RET") #'newline-and-indent)

(defun pes-newline-at-end-of-line ()
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") #'pes-newline-at-end-of-line)


;; The nano style for truncated long lines
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Display line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Enhance the performace of display
(setq display-raw-bytes-as-hex t)
(setq redisplay-skip-fontification-on-input t)


;; Use rainbow delimiters
(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Always show the pointer's position
(setq-default make-pointer-invisible nil)

;; Hide cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; Preserve contents of system clipboard
(setq-default save-interprogram-paste-before-kill t)


(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Page break lines
(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook #'global-page-break-lines-mode)
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
