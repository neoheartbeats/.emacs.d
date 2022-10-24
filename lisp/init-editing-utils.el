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
(setq-default column-number-mode t)
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
(setq-default scroll-preserve-screen-position 'always)
(setq-default set-mark-command-repeat-pop t)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq-default use-short-answers t)
(setq-default line-spacing 0.15)


;; Diable showing line numbers & column in mode line
(setq line-number-mode nil)
(setq column-number-mode nil)


(setq-default fill-column 95)
(global-display-fill-column-indicator-mode 1)


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

;; Auto breaking lines
(auto-fill-mode 1)

;; Recording the changes between windows
(winner-mode 1)


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
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)


;; Use rainbow delimiters
(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode . rainbow-delimiters-mode))


;;; Settings for cursors & clipboard
;; Make the cursor solid
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 1))
(set-cursor-color "#78bf78")

;; The package `beacon.el' also helps refresh the current frame
(use-package beacon
  :custom
  (beacon-color "#78bf78")
  (beacon-lighter "")
  (beacon-size 35)
  :config
  (beacon-mode 1)
  (add-hook 'after-save-hook 'beacon-blink))

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
  :config
  (global-highlight-thing-mode 1))


;;; Literature writing helpers
(use-package powerthesaurus
  :bind
  (("M-p" . powerthesaurus-lookup-synonyms-dwim)))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
