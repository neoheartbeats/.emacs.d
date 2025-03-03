;;; init-system.el --- Configs specific to macOS -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides macOS specific settings.

;;; Code:
;;

;; macOS specified key mapping
;;

(setq mac-option-modifier  'meta)
(setq mac-command-modifier 'super)

;; macOS-styled keybindings
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-c" #'kill-ring-save)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "s-x" #'kill-region)
(keymap-global-set "s-q" #'save-buffers-kill-emacs)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-w" #'kill-current-buffer)
(keymap-global-set "s-e" #'delete-window)
(keymap-global-set "s-r" #'restart-emacs)
(keymap-global-set "s-z" #'undo)
(keymap-global-set "s-d" #'find-file)

(keymap-set emacs-lisp-mode-map "C-c C-c" #'(lambda ()
                                              (interactive)
                                              (let ((debug-on-error t))
                                                (elisp-eval-region-or-buffer))))

;; Set escape key binding
(keymap-global-set "<escape>" #'keyboard-escape-quit)

;; Unset pinch gesture and mouse scaling
(keymap-global-unset "<pinch>")
(keymap-global-unset "<mouse-1>")       ; F11
(keymap-global-unset "<mouse-3>")       ; F12
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

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

(keymap-global-set "s-1" #'delete-other-windows-reversible)
(keymap-global-set "s-2" #'split-window-below-focus)
(keymap-global-set "s-3" #'split-window-right-focus)

;; Use C-Arrow keys to move around windows
(windmove-default-keybindings 'control)

;;; Locate position history
(use-package saveplace
  :config (save-place-mode 1))

(use-package savehist
  :init
  (setq history-length 1024)
  (setq history-delete-duplicates t)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
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

  (setq mark-even-if-inactive nil)
  (setq ring-bell-function 'ignore)
  (setq require-final-newline t)

  (setq-default fill-column 88)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

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
  (setq kill-ring-max 1024)
  (setq kill-whole-line t)

  (setq next-line-add-newlines nil)
  (setq save-interprogram-paste-before-kill t)

  ;; paragraphs.el
  (setq sentence-end-double-space nil)

  ;; prog-mode.el
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  ;; bytecomp.el
  (setq byte-compile-verbose nil)

  (setq warning-minimum-log-level :error)

  ;; enable all commands
  (setq disabled-command-function nil)

  ;; Disable auto copyings
  (setq mouse-drag-copy-region nil)
  (setq select-enable-primary nil)
  (setq select-enable-clipboard t)

  ;; Mouse and scrolling
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively 105)
  (pixel-scroll-precision-mode 1)

  ;; Long lines
  (setq-default truncate-lines t)
  (setq auto-hscroll-mode 'current-line)
  (global-visual-wrap-prefix-mode 1)


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

;; Ignore temporary buffers
(defun sthenno/filtered-cycle-buffer (cycle-func)
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
                (not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun sthenno/cycle-to-next-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'next-buffer))

(defun sthenno/cycle-to-previous-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'previous-buffer))

(keymap-global-set "s-<right>" #'sthenno/cycle-to-next-buffer)
(keymap-global-set "s-<left>"  #'sthenno/cycle-to-previous-buffer)

(provide 'init-system)

;;; init-system.el ends here
