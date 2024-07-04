;;; init-system.e.el --- Configs specific to macOS -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides macOS specific settings.
;;
;;; Code:
;;

;; macOS specified key mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(bind-keys :map global-map
           ("s-a" . mark-whole-buffer)
           ("s-c" . kill-ring-save)
           ("s-l" . goto-line)
           ("s-q" . save-buffers-kill-emacs)
           ("s-s" . save-buffer)
           ("s-v" . yank)
           ("s-w" . kill-current-buffer)
           ("s-e" . delete-window)
           ("s-r" . restart-emacs)
           ("s-z" . undo)
           ("s-d" . find-file)
           ("s-<backspace>" . kill-whole-line))

(bind-keys :map emacs-lisp-mode-map
           ("C-c C-c". eval-buffer))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; "C-i" is treated as the same of "TAB" by default.
;; It is better distinguish it. Note this just makes "C-i" bacome undefined
;; and it still cannot be used as any effective keys [FIXME]
(define-key input-decode-map "\C-i" [C-i])

;; Disable these keys
(global-unset-key (kbd "<pinch>"))

;; Do not scaling frame using mouse
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Move point
(global-unset-key (kbd "C-<up>")) ; `backward-paragraph', `M-<up>' is used instead
(global-set-key (kbd "M-<up>") #'backward-paragraph)

(global-unset-key (kbd "C-<down>")) ; `forward-paragraph', `M-<down>' is used instead
(global-set-key (kbd "M-<down>") #'forward-paragraph)

(global-unset-key (kbd "C-<left>")) ; `left-word', `M-<left>' is used instead
(global-unset-key (kbd "C-<right>")) ; `right-word', `M-<right>' is used instead

;; Split windows
(defun split-window-below-focus ()
  "Like `split-window-below', but focus to the new window after execution."
  (interactive)
  (split-window-below)
  (windmove-down)
  (run-hooks 'split-window-below-focus-hook))

(defun split-window-right-focus ()
  "Like `split-window-right', but focus to the new window after execution."
  (interactive)
  (split-window-right)
  (windmove-right)
  (run-hooks 'split-window-right-focus-hook))

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

;; Move around windows
(global-unset-key (kbd "C-x o")) ; `other-window', `C-o' is used instead
(global-set-key (kbd "C-o") #'other-window)

(bind-keys :map global-map
           ("C-<up>" . windmove-up)
           ("C-<down>" . windmove-down)
           ("C-<left>" . windmove-left)
           ("C-<right>" . windmove-right))


;; Increase how much is read from processes (default is 4kb)
(setq read-process-output-max #x10000)

;; Locate position history
(use-package saveplace
  :config
  (save-place-mode 1))

(use-package savehist
  :init
  (setq history-length 200)
  (setq history-delete-duplicates t)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; Auto saving mechanism
(setq auto-save-interval 2400
      auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-cache-directory))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-cache-directory)))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5)


;; Misc options
(setq use-short-answers t)
(setq dired-use-ls-dired nil)
(setq delete-by-moving-to-trash t)
(setq auto-hscroll-mode 'current-line)
(setq case-fold-search t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq mark-even-if-inactive nil)
(setq ring-bell-function 'ignore)
(setq save-silently t)
(setq set-mark-command-repeat-pop t)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq help-window-select t)
(setq-default fill-column 88)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

(setq vc-follow-symlinks t)
(setq echo-keystrokes-help nil)

(setq locate-command "mdfind")


;;; Global functions
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

;; Pretty print
(defun indent-current-buffer ()
  "Indent current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (run-hooks 'indent-current-buffer-hook))

(defun untabify-current-buffer ()
  "Convert all tabs to multiple spaces for current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)))
  (run-hooks 'untabify-current-buffer-hook))

(defun my/pretty-print-current-buffer ()
  "Pretty print current buffer."
  (interactive)
  (save-excursion
    (indent-current-buffer)
    (untabify-current-buffer)
    (delete-trailing-whitespace))
  (run-hooks 'my/pretty-print-current-buffer-hook))

(add-hook 'before-save-hook #'my/pretty-print-current-buffer)

(global-set-key (kbd "s-p") #'my/pretty-print-current-buffer)

;; To access the `.emacs.d' root
(defun open-emacs-config-dir ()
  "Prompt the user to open a file in the user's Emacs config directory."
  (interactive)
  (let ((default-directory (concat user-emacs-directory "lisp/")))
    (call-interactively 'find-file)))

(global-set-key (kbd "<f12>") 'open-emacs-config-dir)

;; Ignore temporary buffers
(defun my/filtered-cycle-buffer (cycle-func)
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
                (not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun my/cycle-to-next-buffer ()
  (interactive)
  (my/filtered-cycle-buffer 'next-buffer)
  (run-hooks 'my/cycle-to-next-buffer-hook))

(defun my/cycle-to-previous-buffer ()
  (interactive)
  (my/filtered-cycle-buffer 'previous-buffer)
  (run-hooks 'my/cycle-to-previous-buffer-hook))

(bind-keys :map global-map
           ("<s-right>" . my/cycle-to-next-buffer)
           ("<s-left>" . my/cycle-to-previous-buffer))


;; Mouse and scroll settings
(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 105)
(add-hook 'after-init-hook #'(lambda ()
                               (pixel-scroll-precision-mode 1)))


;; Disable auto copyings
(setq mouse-drag-copy-region nil)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq search-default-mode 'char-fold-to-regexp)

(defun my/delete-current-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (run-hooks 'my/delete-current-line-hook))

(defun my/delete-to-beginning-of-line ()
  "Delete from the current position to the beginning of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(global-set-key (kbd "s-<backspace>") 'my/delete-current-line)
(global-set-key (kbd "C-<backspace>") 'my/delete-to-beginning-of-line)


(use-package dired
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-make-directory-clickable t)
  (setq dired-free-space nil)
  (setq dired-mouse-drag-files t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(provide 'init-system)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
