;;; init-system.e.el --- Configs specific to macOS -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides macOS specific settings.
;;

;;; Code:

;; macOS specified key mapping
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
	   ("s-r" . restart-emacs)
	   ("s-z" . undo)
	   ("s-d" . find-file))

(bind-keys :map global-map
	   ("s-1" . delete-other-windows)
	   ("s-2" . split-window-below)
	   ("s-3" . split-window-right)
	   ("s-<backspace>" . kill-whole-line))

(bind-keys :map emacs-lisp-mode-map
	   ("C-c C-c". eval-buffer))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; "C-i" is treated as the same of "TAB" by default.
;; It is better distinguish it. Note this just makes "C-i" bacome undefined
;; and it still cannot be used as any effective keys
(define-key input-decode-map "\C-i" [C-i])

;; "s-[" is used as the prefix key standing for "insert" (see also `init-temp')
(bind-keys :map global-map
	   ("s-[ f" . insert-file))

;; Disable these keys
(global-unset-key (kbd "<pinch>"))

;; Do not scaling frame using mouse
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))


;; Fix environment for macOS
(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))


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

;; Auto saving mechanism
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


;; Misc options
(setq use-short-answers t)
(setq dired-use-ls-dired nil)
(setq auto-hscroll-mode 'current-line)
(setq case-fold-search t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq mark-even-if-inactive nil)
(setq make-pointer-invisible nil)
(setq ring-bell-function 'ignore)
(setq save-silently t)
(setq set-mark-command-repeat-pop t)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq help-window-select t)
(setq xref-search-program 'ripgrep)
(setq fill-column 88)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq require-final-newline t)
(setq inhibit-compacting-font-caches t)


;; Global functions
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

;; Ignore temporary buffers
(defun my/filtered-cycle-buffer (cycle-func)
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
		(not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun my/cycle-to-next-buffer ()
  (interactive)
  (my/filtered-cycle-buffer 'next-buffer))

(defun my/cycle-to-previous-buffer ()
  (interactive)
  (my/filtered-cycle-buffer 'previous-buffer))

(bind-keys :map global-map
	   ("<s-right>" . my/cycle-to-next-buffer)
	   ("<s-left>" . my/cycle-to-previous-buffer))


;; Mouse and scroll settings
(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 95
      make-cursor-line-fully-visible nil)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

;; Disable auto copyings
(setq mouse-drag-copy-region nil)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq search-default-mode 'char-fold-to-regexp)


;;; EMMS
(use-package emms
  :straight t
  :config
  (emms-minimalistic)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/A55/")
  (setq emms-mode-line-icon-enabled-p 'nil)
  (add-hook 'emms-player-started-hook #'emms-shuffle))

(provide 'init-system)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
