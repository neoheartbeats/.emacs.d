;; init-system.el --- Configs specific to macOS -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file provides `macOS' system specific settings.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS specified key mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(bind-keys
 ([(super a)] . mark-whole-buffer)
 ([(super c)] . kill-ring-save)
 ([(super i)] . indent-current-buffer)
 ([(super l)] . goto-line)
 ([(super q)] . save-buffers-kill-emacs)
 ([(super s)] . save-buffer)
 ([(super v)] . yank)
 ([(super w)] . kill-current-buffer)
 ([(super e)] . delete-window)
 ([(super z)] . undo)
 ([(super d)] . find-file))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Disable these keys
(global-unset-key (kbd "<pinch>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Increase how much is read from processes (default is 4kb)
(setq read-process-output-max #x10000)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)
(setq command-line-ns-option-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold most-positive-fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set UTF-8 as the default coding system
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Locate position history
(use-package saveplace :config (save-place-mode 1))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple
              :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The rules of minimalism
(setq line-move-visual nil)

;; Keep cursor at end of lines. This requires `line-move-visual' is nil
(setq track-eol t)

;; Misc options
(setq use-short-answers t)
(setq delete-by-moving-to-trash t)

(setq-default auto-hscroll-mode 'current-line)
(setq-default auto-save-default nil)
(setq-default case-fold-search t)
(setq-default create-lockfiles nil)
(setq-default cursor-in-non-selected-windows nil)
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
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default inhibit-compacting-font-caches t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun indent-current-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

(bind-keys :map global-map
           ("C-x k" . delete-current-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mouse and scroll settings
;;
;; Smoother and nicer scrolling
(setq scroll-step 1)
(setq scroll-conservatively 105)
(setq scroll-margin 15)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed t)
(setq auto-window-vscroll nil)

(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

;; Disable auto copying
(setq mouse-drag-copy-region nil)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq search-default-mode 'char-fold-to-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clipboard
(setq kill-ring-max 250)
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Built-in Sqlite support
(use-package emacsql-sqlite-builtin :ensure t :demand t)


(provide 'init-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-system.el ends here
