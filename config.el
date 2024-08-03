;;; config.el --- sthenno's Emacs config -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Sthenno

;; Author: Sthenno <sthenno@sthenno.com>

;;; Commentary:

;; 

;;; Code:

;;@ bootstrap

(defun sthenno/setup-init ()
  (interactive) 
  (progn
    (setq-local outline-regexp ";;; Code\\|;;@+")
    (setq-local outline-heading-alist '((";;; Code" . 1) (";;@" . 2) (";;@@" . 3)))
    (setq-local outline-minor-mode-use-buttons t)
    (setq-local outline-minor-mode-highlight 'override)
    (setq-local outline-minor-mode-cycle t)
    (setq-local outline-level 'outline-level)
    (outline-minor-mode 1)))

(defvar sthenno-caching-directory (locate-user-emacs-file "caching")
  "Location that files created by Emacs are placed.")

;;@@custom

(setq custom-file (make-temp-file "ciallo_"))

;;@@encoding

(set-language-environment "UTF-8")
(setq default-input-method nil)

;;@@perf-tuning

(use-package emacs
  :config
  (setq-default process-adaptive-read-buffering nil)
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default highlight-nonselected-windows nil)

  (setq-default fast-but-imprecise-scrolling t)
  (setq-default redisplay-skip-fontification-on-input t)
  (setq-default ffap-machine-p-known 'reject)
  (setq-default auto-mode-case-fold nil)

  (setq-default bidi-display-reordering  'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)

  (setq-default inhibit-compacting-font-caches t))

;;@@default-ui

(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Suppress GUI features
  (setq use-dialog-box nil
        use-file-dialog nil)
  (setq inhibit-splash-screen t
        inhibit-startup-buffer-menu t)
  (setq initial-scratch-message nil)
  :config
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;;@@ package-management

(use-package emacs
  :config
  (setq package-archives
        '(("gnu-devel" . "https://elpa.gnu.org/devel/")
          ("gnu"       . "https://elpa.gnu.org/packages/")
          ("nongnu"    . "https://elpa.nongnu.org/nongnu/")
          ("melpa"     . "https://melpa.org/packages/")))
  (setq package-install-upgrade-built-in t
        package-native-compile t))

;;@@ gc-tuning

(use-package gcmh
  :ensure t
  :demand t
  :config
  (defun gcmh-register-idle-gc ()
    "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
    (unless (eq this-command 'self-insert-command)
      (let ((idle-t (if (eq gcmh-idle-delay 'auto)
                        (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
                      gcmh-idle-delay)))
        (if (timerp gcmh-idle-timer)
            (timer-set-time gcmh-idle-timer idle-t)
          (setf gcmh-idle-timer
                (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))))
  (setq gcmh-idle-delay 'auto
        gcmh-high-cons-threshold (* 32 1024 1024))
  (gcmh-mode 1))

;;@@ defaults

(use-package emacs
  :init
  (setq
   kill-do-not-save-duplicates t        ; inhibit duplicates to kill ring
   mouse-yank-at-point t                ; yank text to point, not to mouse cursor
   sentence-end-double-space nil        ; 30mar2023
   idle-update-delay 1.0                ; before updating things on screen
   epg-pinentry-mode 'loopback          ; query passphrases through the minibuffer
   x-stretch-cursor t                   ; Stretch cursor to glyph width
   read-process-output-max (* 64 1024)
   use-short-answers t           
   y-or-n-p-use-read-key t              ; read-key instead of minibuffer
   disabled-command-function nil        ; Allow all the things
   auto-window-vscroll nil
   next-screen-context-lines 4    
   scroll-margin 2                
   scroll-conservatively 50
   scroll-preserve-screen-position t)
  :config
  (pixel-scroll-precision-mode 1))

;;@ system

;;@@ Fix PATH for macOS

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))

;;@@ macOS specified key mapping

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(bind-keys :map global-map
           ("s-a" . mark-whole-buffer)
           ("s-c" . kill-ring-save)
           ("s-q" . save-buffers-kill-emacs)
           ("s-s" . save-buffer)
           ("s-v" . yank)
           ("s-w" . kill-current-buffer)
           ("s-e" . delete-window)
           ("s-r" . restart-emacs)
           ("s-z" . undo)
           ("s-d" . find-file))

(defun sthenno/load-current-file ()
  "Load current file."
  (interactive)
  (load-file (buffer-file-name)))

(keymap-set emacs-lisp-mode-map "C-c C-c" #'sthenno/load-current-file)

(keymap-global-set "<f2>" #'ielm)

(keymap-global-set "<escape>" #'keyboard-escape-quit)

(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")
(keymap-global-unset "C-<left>")
(keymap-global-unset "C-<right>")

(keymap-substitute global-map "C-<up>" "M-<up>")
(keymap-substitute global-map "C-<down>" "M-<down>")

;; Split windows
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

(bind-keys :map global-map
           ("s-1" . delete-other-windows-reversible)
           ("s-2" . split-window-below-focus)
           ("s-3" . split-window-right-focus))

;; Move around windows
(bind-keys :map global-map
           ("C-<up>"    . windmove-up)
           ("C-<down>"  . windmove-down)
           ("C-<left>"  . windmove-left)
           ("C-<right>" . windmove-right))

;; Remember changes on windows
;; Use C-c <left> and C-c <right> to undo and redo changes on windows
(use-package winner
  :init

  ;; Restore windows after finishing Ediff
  (add-hook 'ediff-quit-hook #'winner-undo)
  :config (winner-mode 1))

;; Resizing frames in a smooth way
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Locate position history
(use-package saveplace
  :config (save-place-mode 1))

(use-package savehist
  :init
  (setq history-length 200)
  (setq history-delete-duplicates t)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; Keep track of recently opened files.
(use-package recentf
  :init (setq recentf-save-file (locate-user-emacs-file "recentf")
              recentf-max-saved-items 200
              recentf-auto-cleanup 300)
  :config (recentf-mode 1))

;;@@ Misc QAQ

(use-package emacs
  :init
  (setq auto-hscroll-mode 'current-line)
  (setq mark-even-if-inactive nil)
  (setq ring-bell-function 'ignore)
  (setq require-final-newline t)
  (setq vc-follow-symlinks t)

  (setq-default fill-column 88)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; Still need time to figure out if this has any side-effects
  (setq auto-save-default t)
  (setq save-silently t)

  ;; Real auto-save
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 30)

  ;; files
  (setq create-lockfiles nil)
  (setq make-backup-files nil)

  ;; Disable auto copyings
  (setq mouse-drag-copy-region nil)
  (setq select-enable-primary nil)
  (setq select-enable-clipboard t)

  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-free-space nil)

  ;; `gls' is preferred on macOS
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; Emacs source files
(setq find-function-C-source-directory "/Users/sthenno/Developer/emacs/src/")

;;@@ global functions for accessibility

;; To access the `.emacs.d' root
(defun open-emacs-config-dir ()
  "Prompt the user to open a file in the user's Emacs config directory."
  (interactive)
  (let ((default-directory (locate-user-emacs-file "lisp/")))
    (call-interactively 'find-file)))

(keymap-global-set "<f12>" #'open-emacs-config-dir)

;; Ignore temporary buffers
(defun sthenno/filtered-cycle-buffer (cycle-func)
  (let ((original-buffer (current-buffer)))
    (funcall cycle-func)
    (while (and (string-match-p "\\*.*\\*" (buffer-name))
                (not (eq original-buffer (current-buffer))))
      (funcall cycle-func))))

(defun sthenno/cycle-to-next-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'next-buffer)
  (run-hooks 'sthenno/cycle-to-next-buffer-hook))

(defun sthenno/cycle-to-previous-buffer ()
  (interactive)
  (sthenno/filtered-cycle-buffer 'previous-buffer)
  (run-hooks 'sthenno/cycle-to-previous-buffer-hook))

(bind-keys :map global-map
           ("<s-right>" . sthenno/cycle-to-next-buffer)
           ("<s-left>"  . sthenno/cycle-to-previous-buffer))

;;@ misc
