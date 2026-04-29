;;; init-system.el --- macOS and system behavior -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains operating-system specific settings and general
;; built-in behavior tweaks.

;;; Code:

(setq-default mac-option-modifier 'meta
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

;; macOS-style keybindings
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

(save-place-mode 1)
(savehist-mode 1)

(setopt recentf-max-saved-items 25
        recentf-show-messages nil)
(recentf-mode 1)

(add-hook 'prog-mode-hook #'turn-on-auto-revert-mode)
(pixel-scroll-precision-mode 1)

(setopt ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        yes-or-no-prompt "(真的嘛?) "
        indent-tabs-mode nil
        ad-redefinition-action 'accept
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        kill-ring-max 512
        save-interprogram-paste-before-kill t
        dired-no-confirm t
        dired-recursive-deletes 'always
        dired-movement-style 'cycle)

(provide 'init-system)
