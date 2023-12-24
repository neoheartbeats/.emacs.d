;;; init-system.e.el --- Configs specific to macOS -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides `macOS' system specific settings.
;;

;;; Code:

;;
;; macOS specified key mapping
;;
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(bind-keys :map global-map
 ("s-a" . mark-whole-buffer)
 ("s-c" . kill-ring-save)
 ("s-i" . indent-current-buffer)
 ("s-l" . goto-line)
 ("s-q" . save-buffers-kill-emacs)
 ("s-s". save-buffer)
 ("s-v" . yank)
 ("s-w" . kill-current-buffer)
 ("s-e" . delete-window)
 ("s-z" . undo)
 ("s-d" . find-file))

(bind-keys :map global-map
           ("s-1" . delete-other-windows)
           ("s-2" . split-window-below)
           ("s-3" . split-window-right)
           ("s-<backspace>" . kill-whole-line)
           ("<s-right>" . switch-to-next-buffer)
           ("<s-left>" . switch-to-prev-buffer))

(bind-keys :map emacs-lisp-mode-map
           ("C-c C-c". eval-buffer))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable these keys
(global-unset-key (kbd "<pinch>"))
(global-unset-key (kbd "s-="))
(global-unset-key (kbd "s--"))

;; Increase how much is read from processes (default is 4kb)
(setq read-process-output-max #x10000)

;; Locate position history
(use-package saveplace
  :config (save-place-mode 1))

(use-package savehist 
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

;;
;; Auto saving mechanism
;;
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (dir-concat user-cache-directory "auto-save-list/.saves-"))
(setq backup-directory-alist
      `(("." . ,(dir-concat user-cache-directory "backup")))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5)

;;
;; Misc options
;;
(setq-default use-short-answers t)
(setq-default dired-use-ls-dired nil)
(setq-default auto-hscroll-mode 'current-line)
(setq-default case-fold-search t)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default mark-even-if-inactive nil)
(setq-default make-pointer-invisible nil)
(setq-default ring-bell-function 'ignore)
(setq-default save-silently t)
(setq-default set-mark-command-repeat-pop t)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq-default help-window-select t)
(setq-default xref-search-program 'ripgrep)
(setq-default fill-column 88)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default inhibit-compacting-font-caches t)

;;
;; Global functions
;;
(defun delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun indent-current-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

;; To access the `.emacs.d' root
(defun open-emacs-config-dir ()
  "Open the Emacs configuration directory."
  (interactive)
  (find-file "~/.emacs.d/lisp/"))

(bind-keys :map global-map
           ("C-x k" . delete-current-file)
           ("<f12>" . open-emacs-config-dir))

;;
;; Mouse and scroll settings
;;
(pixel-scroll-precision-mode 1)

;; Disable auto copyings
(setq mouse-drag-copy-region nil)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq search-default-mode 'char-fold-to-regexp)

(provide 'init-system)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
