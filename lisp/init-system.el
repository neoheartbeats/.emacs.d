;;; init-system.e.el --- Configs specific to macOS -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides `macOS' system specific settings.
;;

;;; Code:

;;
;; macOS specified key mapping
;;
(setopt mac-option-modifier 'meta)
(setopt mac-command-modifier 'super)

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
           ("s-<backspace>" . kill-whole-line)
           ("<s-right>" . switch-to-next-buffer)
           ("<s-left>" . switch-to-prev-buffer))

(bind-keys :map emacs-lisp-mode-map
           ("C-c C-c". eval-buffer))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Disable these keys
(global-unset-key (kbd "<pinch>"))

;; Do not scaling frame using mouse
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Increase how much is read from processes (default is 4kb)
(setopt read-process-output-max #x10000)

;; Locate position history
(use-package saveplace
  :config (save-place-mode 1))

(use-package savehist 
  :config
  (setopt savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setopt history-length 1000)
  (setopt history-delete-duplicates t)
  (setopt savehist-save-minibuffer-history t)
  (savehist-mode 1))

;;
;; Auto saving mechanism
;;
(setopt auto-save-interval 2400)
(setopt auto-save-timeout 300)
(setopt auto-save-list-file-prefix
      (dir-concat user-cache-directory "auto-save-list/.saves-"))
(setopt backup-directory-alist
      `(("." . ,(dir-concat user-cache-directory "backup")))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5)

;;
;; Misc options
;;
(setopt use-short-answers t)
(setopt dired-use-ls-dired nil)
(setopt auto-hscroll-mode 'current-line)
(setopt case-fold-search t)
(setopt create-lockfiles nil)
(setopt make-backup-files nil)
(setopt mark-even-if-inactive nil)
(setopt make-pointer-invisible nil)
(setopt ring-bell-function 'ignore)
(setopt save-silently t)
(setopt set-mark-command-repeat-pop t)
(setopt truncate-lines nil)
(setopt truncate-partial-width-windows nil)
(setopt help-window-select t)
(setopt xref-search-program 'ripgrep)
(setopt fill-column 88)
(setopt tab-width 4)
(setopt indent-tabs-mode nil)
(setopt require-final-newline t)
(setopt inhibit-compacting-font-caches t)

;;;
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

;; Helpful functions to access config files
(defun my/make-quick-config-link (label link)
  (insert "• ")
  (insert label "  􀄫  ")
  (insert-button link
                 'action (lambda (_)
                           (find-file link))
                 'follow-link t)
  (insert "\n"))

(defun my/open-quick-config-links ()
  (interactive)
  (let ((buffer (get-buffer-create "*Config Links*"))
        (configs '(("Zsh  " . "~/.zshrc")
                   ("SSH  " . "~/.ssh/config")
                   ("Emacs" . "~/.emacs.d/"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "\n Configs\n\n")
      (let ((config-overlay (make-overlay 2 10)))
        (overlay-put config-overlay 'face 'org-level-4))
      (mapcar (lambda (item)
                (my/make-quick-config-link (car item) (cdr item)))
              configs))
    (pop-to-buffer buffer t)))

(bind-keys :map global-map
           ("<f2>" . my/open-quick-config-links))

;;
;; Mouse and scroll settings
;;
(add-hook 'after-init-hook #'(lambda ()
                               (pixel-scroll-precision-mode 1)))

;; Disable auto copyings
(setopt mouse-drag-copy-region nil)
(setopt select-enable-primary nil)
(setopt select-enable-clipboard t)
(setopt search-default-mode 'char-fold-to-regexp)

(provide 'init-system)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
