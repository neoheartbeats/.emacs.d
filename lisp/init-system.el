;;; init-system.e.el --- Configs specific to macOS -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides macOS specific settings.

;;; Code:
;;

;; macOS specified key mapping
;;

(setq mac-option-modifier  'meta)
(setq mac-command-modifier 'super)

(bind-keys :map global-map
           ("s-a" . mark-whole-buffer)
           ("s-c" . kill-ring-save)
           ("s-q" . save-buffers-kill-emacs)
           ("s-s" . save-buffer)
           ("s-v" . yank)
           ("s-w" . kill-current-buffer)
           ("s-e" . delete-window)
           ("s-r" . restart-emacs)
           ("s-z" . undo)
           ("s-d" . find-file))

;;; To use a familier undo-redo mechanism

;; Note this global mode directly remaps the default keymaps
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(keymap-set emacs-lisp-mode-map "C-c C-c" #'(lambda ()
                                              (interactive)
                                              (let ((debug-on-error t))
                                                (elisp-eval-region-or-buffer))))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Disable these keys
(global-unset-key (kbd "<pinch>"))

;; Do not scaling frame using mouse
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;;; Split windows
(defun split-window-below-focus ()
  "Like `split-window-below', but focus to the new window after execution."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-focus ()
  "Like `split-window-right', but focus to the new window after execution."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun delete-other-windows-reversible ()
  "Like `delete-other-windows', but can be reserved.
Activate again to undo this. If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(bind-keys :map global-map
           ("s-1" . delete-other-windows-reversible)
           ("s-2" . split-window-below-focus)
           ("s-3" . split-window-right-focus))

;; Use C-Arrow keys to move around windows
(windmove-default-keybindings 'control)

;; Remember changes on windows
;; Use C-c <left> and C-c <right> to undo and redo changes on windows
(use-package winner
  :config (winner-mode 1))

;;; Locate position history
(use-package saveplace
  :config (save-place-mode 1))

(use-package savehist
  :init
  (setq history-length 200)
  (setq history-delete-duplicates t)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package autorevert
  :config
  (setopt auto-revert-use-notify nil)
  (global-auto-revert-mode 1))

;;; Misc options

(use-package emacs
  :demand t
  :init
  (setq use-short-answers t)

  (setq auto-hscroll-mode 'current-line)
  (setq mark-even-if-inactive nil)
  (setq ring-bell-function 'ignore)
  (setq require-final-newline t)
  (setq vc-follow-symlinks t)

  (setq-default fill-column 88)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; Still need time to figure out if this has any side-effects
  (setq auto-save-default nil)
  (setq save-silently t)

  ;; Real auto-save
  (auto-save-visited-mode -1)

  ;; advice.el
  (setq ad-redefinition-action 'accept)

  ;; files.el
  (setq create-lockfiles nil)
  (setq make-backup-files nil)

  (setq delete-old-versions t)
  (setq trash-directory "~/.Trash")

  ;; simple.el
  (setq backward-delete-char-untabify-method 'hungry)
  (setq column-number-mode nil)
  (setq line-number-mode nil)
  
  (setq kill-do-not-save-duplicates t)
  (setq kill-ring-max 500)
  (setq kill-whole-line t)
  
  (setq next-line-add-newlines nil)
  (setq save-interprogram-paste-before-kill t)

  ;; paragraphs.el
  (setq sentence-end-double-space nil)

  ;; prog-mode.el
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  ;; bytecomp.el
  (setq byte-compile-verbose nil)

  ;; warnings.el
  (setq warning-minimum-log-level :error)

  ;; enable all commands
  (setq disabled-command-function nil)

  ;; Mouse and scrolling
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively 105)
  (pixel-scroll-precision-mode 1)

  ;; Disable auto copyings
  (setq mouse-drag-copy-region nil)
  (setq select-enable-primary nil)
  (setq select-enable-clipboard t)
  (setq x-stretch-cursor t)

  ;; dired.el
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-free-space nil)
  (setq dired-clean-up-buffers-too nil)
  (setq dired-dwim-target t)
  (setq dired-hide-details-hide-information-lines nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-lah")
  (setq dired-mouse-drag-files t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy hardlink symlink touch))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-vc-rename-file t)
  
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; `gls' is preferred on macOS
  (setq insert-directory-program "/opt/homebrew/bin/gls")

  :config

  ;; Default is RET
  (define-key input-decode-map [?\C-m] [C-m]))

;;; Global functions for accessibility

;; To access the `.emacs.d' root
(defun open-emacs-config-dir ()
  "Prompt the user to open a file in the user's Emacs config directory."
  (interactive)
  (let ((default-directory (concat user-emacs-directory "lisp/")))
    (call-interactively 'find-file)))

(global-set-key (kbd "<f12>") 'open-emacs-config-dir)

;; Ignore temporary buffers
(defun sthenno/filtered-cycle-buffer (cycle-func)
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
                (not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun sthenno/cycle-to-next-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'next-buffer)
  (run-hooks 'sthenno/cycle-to-next-buffer-hook))

(defun sthenno/cycle-to-previous-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'previous-buffer)
  (run-hooks 'sthenno/cycle-to-previous-buffer-hook))

(bind-keys :map global-map
           ("<s-right>" . sthenno/cycle-to-next-buffer)
           ("<s-left>"  . sthenno/cycle-to-previous-buffer))

(provide 'init-system)
