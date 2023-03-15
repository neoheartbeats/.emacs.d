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
  ([(super f)] . find-file))

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
;; Fix PATH
(use-package exec-path-from-shell :ensure t
  :init
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Increase how much is read from processes (default is 4kb)
(setq read-process-output-max #x10000) ; 64kb

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)
(setq command-line-ns-option-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage Collector Magic Hack
(use-package gcmh :ensure t
  :diminish (gcmh-mode)
  :hook (emacs-startup . gcmh-mode)
  :custom
  ((gcmh-idle-delay 'auto)
    (gcmh-auto-idle-delay-factor 10)
    (gcmh-high-cons-threshold most-positive-fixnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set UTF-8 as the default coding system
(set-charset-priority 'unicode)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better compatibilities
(use-package compat :ensure t :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Locate position history
(use-package saveplace
  :hook
  (after-init . save-place-mode))

(use-package savehist
  :init
  (setq history-length 1500)
  :hook
  (after-init . savehist-mode))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
            (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

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
(setq dired-use-ls-dired nil)

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

(setq-default major-mode 'org-mode)
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default lisp-indent-offset 2)
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

(defun pes-find-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(bind-keys :map global-map
  ("C-x k" . delete-current-file)
  ("<f12>" . pes-find-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mouse and scroll settings
;;
;; Smoother and nicer scrolling
(setq scroll-step 1)
(setq scroll-conservatively 15)
(setq scroll-margin 15)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-scroll-amount '(1
                                   ((shift) . 5)
                                   ((control))))

(add-hook 'after-init-hook #'(lambda ()
                               (pixel-scroll-precision-mode 1)))

;; Disable auto copying
(setq mouse-drag-copy-region nil)
(setq search-default-mode 'char-fold-to-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Built-in Sqlite support
(use-package emacsql-sqlite-builtin :ensure t :demand t)

(provide 'init-system)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-system.el ends here