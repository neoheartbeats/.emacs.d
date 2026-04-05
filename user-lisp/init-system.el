;;; init-system.el --- macOS and system behavior -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains operating-system specific settings and general
;; built-in behavior tweaks.

;;; Code:

(setopt mac-option-modifier 'meta
        mac-command-modifier 'super)

(defun sthenno/eval-elisp-buffer-or-region ()
  "Evaluate the active region or current buffer with `debug-on-error'."
  (interactive)
  (let ((debug-on-error t))
    (elisp-eval-region-or-buffer)))

(defun sthenno/split-window-below-focus ()
  "Split the selected window below and move focus there."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun sthenno/split-window-right-focus ()
  "Split the selected window right and move focus there."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun sthenno/delete-other-windows-reversible ()
  "Like `delete-other-windows', but restore the old layout when repeated."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defun sthenno/filtered-cycle-buffer (cycle-func)
  "Use CYCLE-FUNC while skipping temporary star buffers."
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
                (not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun sthenno/cycle-to-next-buffer ()
  "Move to the next non-temporary buffer."
  (interactive)
  (sthenno/filtered-cycle-buffer #'next-buffer))

(defun sthenno/cycle-to-previous-buffer ()
  "Move to the previous non-temporary buffer."
  (interactive)
  (sthenno/filtered-cycle-buffer #'previous-buffer))

;; macOS-style keybindings.
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-c" #'kill-ring-save)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "s-x" #'kill-region)
(keymap-global-set "s-q" #'kill-emacs)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-w" #'kill-current-buffer)
(keymap-global-set "s-e" #'delete-window)
(keymap-global-set "s-z" #'undo)
(keymap-global-set "s-d" #'find-file)
(keymap-global-set "s-1" #'sthenno/delete-other-windows-reversible)
(keymap-global-set "s-2" #'sthenno/split-window-below-focus)
(keymap-global-set "s-3" #'sthenno/split-window-right-focus)
(keymap-global-set "s-<right>" #'sthenno/cycle-to-next-buffer)
(keymap-global-set "s-<left>" #'sthenno/cycle-to-previous-buffer)
(keymap-global-set "<escape>" #'keyboard-escape-quit)

(keymap-set emacs-lisp-mode-map "C-c C-c" #'sthenno/eval-elisp-buffer-or-region)

;; Disable gestures and mouse bindings that conflict with the preferred workflow.
(keymap-global-unset "<pinch>")
(keymap-global-unset "<mouse-1>")
(keymap-global-unset "<mouse-3>")
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(windmove-default-keybindings 'control)

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

(use-package savehist
  :ensure nil
  :init
  (setopt history-length 128
          history-delete-duplicates t
          savehist-file (locate-user-emacs-file "savehist")
          savehist-save-minibuffer-history t)
  :config
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setopt recentf-max-saved-items 100
          recentf-save-file-modes nil
          recentf-keep nil
          recentf-auto-cleanup nil
          recentf-initialize-file-name-history nil
          recentf-filename-handlers nil
          recentf-show-file-shortcuts-flag nil))

(use-package autorevert
  :ensure nil
  :config
  (setopt auto-revert-use-notify nil
          global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-package emacs
  :ensure nil
  :demand t
  :init
  (setopt use-short-answers t
          mark-even-if-inactive nil
          ring-bell-function #'ignore
          require-final-newline t
          ad-redefinition-action 'accept
          delete-old-versions t
          trash-directory "~/.Trash"
          backward-delete-char-untabify-method 'hungry
          column-number-mode nil
          line-number-mode nil
          kill-do-not-save-duplicates t
          kill-ring-max 512
          kill-whole-line t
          next-line-add-newlines nil
          save-interprogram-paste-before-kill t
          sentence-end-double-space nil
          prettify-symbols-unprettify-at-point 'right-edge
          byte-compile-verbose nil
          warning-minimum-log-level :error
          disabled-command-function nil
          mouse-drag-copy-region nil
          select-enable-primary nil
          select-enable-clipboard t
          dired-auto-revert-buffer #'dired-directory-changed-p
          dired-kill-when-opening-new-dired-buffer t
          dired-free-space nil
          dired-clean-up-buffers-too nil
          dired-dwim-target t
          dired-hide-details-hide-information-lines nil
          dired-hide-details-hide-symlink-targets nil
          dired-listing-switches "-lah"
          dired-mouse-drag-files t
          dired-no-confirm '(byte-compile chgrp chmod chown copy hardlink symlink
                                          touch)
          dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-vc-rename-file t
          dired-movement-style 'cycle-files)
  (setq-default fill-column 88
                tab-width 4
                indent-tabs-mode nil
                truncate-lines t
                auto-hscroll-mode 'current-line)
  (global-visual-wrap-prefix-mode 1)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init (setopt scroll-margin 0
                scroll-conservatively 105)
  :config (ultra-scroll-mode 1))

(provide 'init-system)
