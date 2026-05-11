;;; init-system.el --- macOS and system behavior -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains operating-system specific settings and general built-in behavior tweaks.

;;; Code:

(setopt mac-option-modifier 'meta
        mac-command-modifier 'super)
(setopt switch-to-prev-buffer-skip 0)

(keymap-global-set "s-q" #'kill-emacs)
(keymap-global-set "s-w" #'kill-current-buffer)
(keymap-global-set "s-e" #'delete-window)
(keymap-global-set "s-d" #'find-file)
(keymap-global-set "s-<right>" #'switch-to-next-buffer)
(keymap-global-set "s-<left>" #'switch-to-prev-buffer)
(keymap-global-set "<escape>" #'keyboard-escape-quit)
(keymap-set emacs-lisp-mode-map "C-c C-c" #'emacs-lisp-native-compile-and-load)

(setopt save-place-autosave-interval 300)
(save-place-mode 1)
(savehist-mode 1)

(setopt recentf-max-saved-items 25
        recentf-autosave-interval 300
        recentf-show-messages nil
        recentf-suppress-open-file-help t)
(recentf-mode 1)

(add-hook 'prog-mode-hook #'turn-on-auto-revert-mode)
(pixel-scroll-precision-mode 1)

(setopt electric-indent-actions '(yank before-save))
(electric-pair-mode 1)
(electric-indent-mode 1)

(delete-selection-mode 1)

(defun sthenno/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun sthenno/delete-to-beginning-of-line ()
  "Delete text from point to the beginning of the current line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(keymap-global-set "C-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)
(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>" #'backward-paragraph)


(setopt elisp-fontify-semantically t)
(setopt abbrev-mode t)

(defun sthenno/lisp-indent-buffer ()
  "Indent the current Lisp buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((inhibit-message t))
        (lisp-indent-region (point-min) (point-max))))))
(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/lisp-indent-buffer)

(setopt ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        indent-tabs-mode nil
        insert-directory-program (executable-find "gls")
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        kill-ring-max 256
        copy-region-blink-delay 0
        copy-region-blink-predicate #'ignore
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t)
(setopt dired-no-confirm t
        dired-use-ls-dired t
        dired-hide-details-hide-information-lines t
        dired-hide-details-hide-absolute-location t
        dired-check-symlinks nil
        dired-recursive-deletes 'always
        dired-movement-style 'cycle)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'init-system)
